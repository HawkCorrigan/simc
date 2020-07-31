#pragma once
#include "simulationcraft.hpp"
#include "player/covenant.hpp"
#include "player/pet_spawner.hpp"

namespace shaman
{
typedef std::pair<std::string, simple_sample_data_with_min_max_t> data_t;
typedef std::pair<std::string, simple_sample_data_t> simple_data_t;
struct shaman_t;

enum totem_e
{
  TOTEM_NONE = 0,
  TOTEM_AIR,
  TOTEM_EARTH,
  TOTEM_FIRE,
  TOTEM_WATER,
  TOTEM_MAX
};

enum wolf_type_e
{
  SPIRIT_WOLF = 0,
  FIRE_WOLF,
  FROST_WOLF,
  LIGHTNING_WOLF
};

enum class elemental
{
  FIRE,
  EARTH,
  STORM,
};

enum imbue_e
{
  IMBUE_NONE = 0,
  FLAMETONGUE_IMBUE,
  WINDFURY_IMBUE,
  FROSTBRAND_IMBUE,
  EARTHLIVING_IMBUE
};

struct shaman_attack_t;
struct shaman_spell_t;
struct shaman_heal_t;
struct shaman_totem_pet_t;
struct totem_pulse_event_t;
struct totem_pulse_action_t;

struct shaman_td_t : public actor_target_data_t
{
  struct dots
  {
    dot_t* flame_shock;
  } dot;

  struct debuffs
  {
    // Elemental
  } debuff;

  struct heals
  {
    dot_t* riptide;
    dot_t* earthliving;
  } heal;
  
  shaman_td_t( player_t* target, shaman_t* p );

  shaman_t* actor() const
  {
    return debug_cast<shaman_t*>( source );
  }
};

struct counter_t
{
  const sim_t* sim;

  double value, interval;
  timespan_t last;

  counter_t( shaman_t* p );

  void add( double val )
  {
    // Skip iteration 0 for non-debug, non-log sims
    if ( sim->current_iteration == 0 && sim->iterations > sim->threads && !sim->debug && !sim->log )
      return;

    value += val;
    if ( last > timespan_t::min() )
      interval += ( sim->current_time() - last ).total_seconds();
    last = sim->current_time();
  }

  void reset()
  {
    last = timespan_t::min();
  }

  double divisor() const
  {
    if ( !sim->debug && !sim->log && sim->iterations > sim->threads )
      return sim->iterations - sim->threads;
    else
      return std::min( sim->iterations, sim->threads );
  }

  double mean() const
  {
    return value / divisor();
  }

  double interval_mean() const
  {
    return interval / divisor();
  }

  void merge( const counter_t& other )
  {
    value += other.value;
    interval += other.interval;
  }
};


struct shaman_t : public player_t
{
public:
  // Misc
  bool lava_surge_during_lvb; //to ele
  std::vector<counter_t*> counters;
  /// Shaman ability cooldowns
  std::vector<cooldown_t*> ability_cooldowns;
  player_t* recent_target =
      nullptr;  // required for Earthen Rage, whichs' ticks damage the most recently attacked target


  // Data collection for cooldown waste
  auto_dispose<std::vector<data_t*> > cd_waste_exec, cd_waste_cumulative;
  auto_dispose<std::vector<simple_data_t*> > cd_waste_iter;

  // Cached actions
  struct actions_t
  {
    spell_t* lightning_shield; 
    spell_t* earthen_rage; 
  } action;

  // Pets
  struct pets_t
  {
    pet_t* pet_fire_elemental;
    pet_t* pet_storm_elemental;
    pet_t* pet_earth_elemental;

    pet_t* guardian_fire_elemental;
    pet_t* guardian_storm_elemental;
    pet_t* guardian_earth_elemental;

    // spawner::pet_spawner_t<pet_t, shaman_t> ember_elemental;
    // spawner::pet_spawner_t<pet_t, shaman_t> spark_elemental;

    pets_t( shaman_t* );
  } pet;

  // Constants
  struct
  {
    double matching_gear_multiplier;
  } constant;

  // Buffs
  struct buffs_t
  {
    // shared between all three specs
    buff_t* ascendance;
    buff_t* ghost_wolf;

    // Elemental, Restoration
    buff_t* lava_surge;

    // Elemental
    buff_t* earthen_rage;
    buff_t* master_of_the_elements;
    buff_t* surge_of_power;
    buff_t* icefury;
    buff_t* unlimited_power;
    buff_t* stormkeeper;
    stat_buff_t* elemental_blast_crit;
    stat_buff_t* elemental_blast_haste;
    stat_buff_t* elemental_blast_mastery;
    buff_t* wind_gust;  // Storm Elemental passive 263806

    // Restoration
    buff_t* spirit_walk;
    buff_t* spiritwalkers_grace;
    buff_t* tidal_waves;

    // PvP
    buff_t* thundercharge;

  } buff;

  // Cooldowns
  struct cooldowns_t
  {
    cooldown_t* ascendance;
    cooldown_t* fire_elemental;
    cooldown_t* feral_spirits;
    cooldown_t* lava_burst;
    cooldown_t* storm_elemental;
    cooldown_t* strike;  // shared CD of Storm Strike and Windstrike
  } cooldown;

  // Gains
  struct gains_t
  {
    gain_t* aftershock;
    gain_t* resurgence;
    gain_t* fire_elemental;
    gain_t* spirit_of_the_maelstrom;
  } gain;

  // Tracked Procs
  struct procs_t
  {
    // Elemental, Restoration
    proc_t* lava_surge;
    proc_t* wasted_lava_surge;
    proc_t* surge_during_lvb;
  } proc;

  // Class Specializations
  struct specializations_t
  {
    // Generic
    const spell_data_t* mail_specialization;
    const spell_data_t* shaman;

    // Elemental
    const spell_data_t* chain_lightning_2;  // 7.1 Chain Lightning additional 2 targets passive
    const spell_data_t* elemental_fury;     // general crit multiplier
    const spell_data_t* elemental_shaman;   // general spec multiplier
    const spell_data_t* lava_burst_2;       // 7.1 Lava Burst autocrit with FS passive
    const spell_data_t* lava_surge;

    // Restoration
    const spell_data_t* purification;
    const spell_data_t* resurgence;
    const spell_data_t* riptide;
    const spell_data_t* tidal_waves;
    const spell_data_t* spiritwalkers_grace;
    const spell_data_t* restoration_shaman;  // general spec multiplier
  } spec;

  // Masteries
  struct
  {
    const spell_data_t* elemental_overload;
    const spell_data_t* deep_healing;
  } mastery;

  // Talents
  struct talents_t
  {
    // Generic / Shared
    const spell_data_t* elemental_blast;
    const spell_data_t* totem_mastery;
    const spell_data_t* spirit_wolf;
    const spell_data_t* earth_shield;
    const spell_data_t* static_charge;
    const spell_data_t* natures_guardian;
    const spell_data_t* wind_rush_totem;
    const spell_data_t* stormkeeper;
    const spell_data_t* ascendance;

    // Elemental
    const spell_data_t* earthen_rage;
    const spell_data_t* echo_of_the_elements;
    // elemental blast - shared

    const spell_data_t* aftershock;
    // echoing shock
    // totem mastery - shared

    // spirit wolf - shared
    // earth shield - shared
    // static charge - shared

    const spell_data_t* master_of_the_elements;
    const spell_data_t* storm_elemental;
    const spell_data_t* liquid_magma_totem;

    // natures guardian - shared
    const spell_data_t* ancestral_guidance;
    // wind rush totem - shared

    const spell_data_t* surge_of_power;
    const spell_data_t* primal_elementalist;
    const spell_data_t* icefury;

    const spell_data_t* unlimited_power;
    // stormkeeper - shared
    // ascendance - shared

    // Restoration
    const spell_data_t* graceful_spirit;
  } talent;

  // Misc Spells
  struct misc_t
  {
    const spell_data_t* resurgence;
    const spell_data_t* fire_elemental;
    const spell_data_t* storm_elemental;
    const spell_data_t* earth_elemental;
  } spell;

  // Cached pointer for ascendance / normal white melee
  shaman_attack_t* melee_mh;
  shaman_attack_t* melee_oh;


  shaman_t( sim_t* sim, util::string_view name, race_e r = RACE_TAUREN )
    : player_t( sim, SHAMAN, name, r ),
      lava_surge_during_lvb( false ),
      action(),
      pet( this ),
      constant(),
      buff(),
      cooldown(),
      gain(),
      proc(),
      spec(),
      mastery(),
      talent(),
      spell()
  {
    /*
    range::fill( pet.spirit_wolves, nullptr );
    range::fill( pet.elemental_wolves, nullptr );
    */

    // Cooldowns
    cooldown.ascendance      = get_cooldown( "ascendance" );
    cooldown.fire_elemental  = get_cooldown( "fire_elemental" );
    cooldown.storm_elemental = get_cooldown( "storm_elemental" );
    cooldown.lava_burst      = get_cooldown( "lava_burst" );
    

    melee_mh      = nullptr;
    melee_oh      = nullptr;

    if ( specialization() == SHAMAN_ELEMENTAL)
      resource_regeneration = regen_type::DISABLED;
    else
      resource_regeneration = regen_type::DYNAMIC;
  }

  ~shaman_t() override;

  // Misc
  bool active_elemental_pet() const;
  void summon_fire_elemental( timespan_t duration, bool essence_proc );
  void summon_storm_elemental( timespan_t duration, bool essence_proc );

  // triggers
  void trigger_maelstrom_gain( double base, gain_t* gain = nullptr );
  void trigger_windfury_weapon( const action_state_t* );
  void trigger_flametongue_weapon( const action_state_t* );
  void trigger_icy_edge( const action_state_t* );
  void trigger_stormbringer( const action_state_t* state, double proc_chance = -1.0, proc_t* proc_obj = nullptr );
  void trigger_lightning_shield( const action_state_t* state );
  void trigger_hot_hand( const action_state_t* state );

  // Legendary
  // empty - for now

  // Character Definition
  void init_spells() override;
  void init_base_stats() override;
  void init_scaling() override;
  void create_buffs() override;
  void create_actions() override;
  void create_options() override;
  void init_gains() override;
  void init_procs() override;

  // APL releated methods
  void init_action_list() override;
  void init_action_list_enhancement();
  void init_action_list_elemental();
  void init_action_list_restoration_dps();
  std::string generate_bloodlust_options();
  std::string default_potion() const override;
  std::string default_flask() const override;
  std::string default_food() const override;
  std::string default_rune() const override;

  void init_rng() override;
  void init_special_effects() override;

  double resource_loss( resource_e resource_type, double amount, gain_t* g = nullptr, action_t* a = nullptr ) override;
  void moving() override;
  void invalidate_cache( cache_e c ) override;
  double temporary_movement_modifier() const override;
  double passive_movement_modifier() const override;
  double composite_melee_crit_chance() const override;
  double composite_melee_haste() const override;
  double composite_melee_speed() const override;
  double composite_attack_power_multiplier() const override;
  double composite_attribute_multiplier( attribute_e ) const override;
  double composite_spell_crit_chance() const override;
  double composite_spell_haste() const override;
  double composite_spell_power( school_e school ) const override;
  double composite_spell_power_multiplier() const override;
  double composite_player_multiplier( school_e school ) const override;
  double composite_player_target_multiplier( player_t* target, school_e school ) const override;
  double composite_player_pet_damage_multiplier( const action_state_t* state ) const override;
  double composite_maelstrom_gain_coefficient( const action_state_t* state = nullptr ) const;
  double matching_gear_multiplier( attribute_e attr ) const override;
  action_t* create_action( util::string_view name, const std::string& options ) override;
  pet_t* create_pet( util::string_view name, util::string_view type = "" ) override;
  void create_pets() override;
  std::unique_ptr<expr_t> create_expression( util::string_view name ) override;
  resource_e primary_resource() const override
  {
    return RESOURCE_MANA;
  }
  role_e primary_role() const override;
  stat_e convert_hybrid_stat( stat_e s ) const override;
  void arise() override;
  void combat_begin() override;
  void reset() override;
  void merge( player_t& other ) override;
  void copy_from( player_t* ) override;

  void datacollection_begin() override;
  void datacollection_end() override;

  target_specific_t<shaman_td_t> target_data;

  shaman_td_t* get_target_data( player_t* target ) const override
  {
    shaman_td_t*& td = target_data[ target ];
    if ( !td )
    {
      td = new shaman_td_t( target, const_cast<shaman_t*>( this ) );
    }
    return td;
  }

  // Helper to trigger a secondary ability through action scheduling (i.e., schedule_execute()),
  // without breaking targeting information. Note, causes overhead as an extra action_state_t object
  // is needed to carry the information.
  void trigger_secondary_ability( const action_state_t* source_state, action_t* secondary_action,
                                  bool inherit_state = false );

  template <typename T_CONTAINER, typename T_DATA>
  T_CONTAINER* get_data_entry( const std::string& name, std::vector<T_DATA*>& entries )
  {
    for ( size_t i = 0; i < entries.size(); i++ )
    {
      if ( entries[ i ]->first == name )
      {
        return &( entries[ i ]->second );
      }
    }

    entries.push_back( new T_DATA( name, T_CONTAINER() ) );
    return &( entries.back()->second );
  }
};

shaman_t::~shaman_t()
{
  range::dispose( counters );
}

counter_t::counter_t( shaman_t* p ) : sim( p->sim ), value( 0 ), interval( 0 ), last( timespan_t::min() )
{
  p->counters.push_back( this );
}


// ==========================================================================
// Shaman Custom Buff Declaration
// ==========================================================================
//

struct ascendance_buff_t : public buff_t
{
  ascendance_buff_t( shaman_t* p );
  void ascendance( attack_t* mh, attack_t* oh );
  bool trigger( int stacks, double value, double chance, timespan_t duration ) override;
  void expire_override( int expiration_stacks, timespan_t remaining_duration ) override;
};

shaman_td_t::shaman_td_t( player_t* target, shaman_t* p ) : actor_target_data_t( target, p )
{
  // Shared
  dot.flame_shock = target->get_dot( "flame_shock", p );

  // Elemental
  
}




}