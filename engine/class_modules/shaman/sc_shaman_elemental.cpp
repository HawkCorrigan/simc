#include "simulationcraft.hpp"
#include "sc_shaman.hpp"

#include "simulationcraft.hpp"

#include "sc_shaman.hpp"

namespace shaman
{
struct shaman_elemental_spell_t : shaman_spell_t
{
  action_t* overload;

public:
  bool affected_by_master_of_the_elements = false;
  bool affected_by_stormkeeper            = false;

  shaman_elemental_spell_t( const std::string& token, shaman_t* p, const spell_data_t* s = spell_data_t::nil(),
                            const std::string& options = std::string() )
    : shaman_spell_t( token, p, s, options )
  {
  }

  double action_multiplier() const override
  {
    double m = shaman_action_t::action_multiplier();
    // BfA Elemental talent - Master of the Elements
    if ( affected_by_master_of_the_elements )
    {
      m *= 1.0 + p()->buff.master_of_the_elements->value();
    }
    return m;
  }

  void execute() override
  {
    shaman_spell_base_t::execute();

    if ( p()->talent.earthen_rage->ok() && !background /*&& execute_state->action->harmful*/ )
    {
      p()->recent_target = execute_state->target;
      p()->buff.earthen_rage->trigger();
    }

    // BfA Elemental talent - Master of the Elements
    if ( affected_by_master_of_the_elements && !background )
    {
      p()->buff.master_of_the_elements->decrement();
    }
  }

  void schedule_travel( action_state_t* s ) override
  {
    trigger_elemental_overload( s );

    shaman_spell_base_t::schedule_travel( s );
  }

  virtual double overload_chance( const action_state_t* ) const
  {
    return p()->cache.mastery_value();
  }

  // Additional guaranteed overloads
  virtual size_t n_overloads( const action_state_t* ) const
  {
    return 0;
  }

  // Additional overload chances
  virtual size_t n_overload_chances( const action_state_t* ) const
  {
    return 0;
  }

  void trigger_elemental_overload( const action_state_t* source_state ) const
  {
    struct elemental_overload_event_t : public event_t
    {
      action_state_t* state;

      elemental_overload_event_t( action_state_t* s )
        : event_t( *s->action->player, timespan_t::from_millis( 400 ) ), state( s )
      {
      }

      ~elemental_overload_event_t() override
      {
        if ( state )
          action_state_t::release( state );
      }

      const char* name() const override
      {
        return "elemental_overload_event_t";
      }

      void execute() override
      {
        state->action->schedule_execute( state );
        state = nullptr;
      }
    };

    if ( !p()->mastery.elemental_overload->ok() )
    {
      return;
    }

    if ( !overload )
    {
      return;
    }

    /* Hacky to recreate ingame behavior. Stormkeeper forces only the first overload to happen. */
    unsigned overloads = rng().roll( overload_chance( source_state ) );

    if ( p()->buff.stormkeeper->up() && affected_by_stormkeeper )
    {
      overloads = 1;
    }

    overloads += (unsigned)n_overloads( source_state );

    for ( size_t i = 0, end = overloads; i < end; ++i )
    {
      action_state_t* s = overload->get_state();
      overload->snapshot_state( s, result_amount_type::DMG_DIRECT );
      s->target = source_state->target;

      make_event<elemental_overload_event_t>( *sim, s );
    }
  }
};

struct elemental_td_t : public shaman_td_t
{
public:
  shaman_td_t* get()
  {
    return this;
  }

  struct dots : shaman_td_t::dots
  {
    dot_t* flameshock;
  } dot;

  struct debuffs : shaman_td_t::debuffs
  {
  } debuff;

  elemental_td_t( player_t* target, elemental_shaman_t* p );
};

struct elemental_shaman_t : public shaman_t
{
  struct actions_t : shaman_t::actions_t
  {
    spell_t* earthen_rage;
  } action;

  struct buffs_t : shaman_t::buffs_t
  {
    // talents
    buff_t* earthen_rage;
    buff_t* echoing_shock;
    buff_t* master_of_the_elements;
    buff_t* surge_of_power;
    buff_t* icefury;
    buff_t* unlimited_power;
    buff_t* stormkeeper;
    stat_buff_t* elemental_blast_crit;
    stat_buff_t* elemental_blast_haste;
    stat_buff_t* elemental_blast_mastery;
    buff_t* wind_gust;

    // Totem Mastery?

    // Leggos
  };

  struct cooldowns_t : shaman_t::cooldowns_t
  {
    cooldown_t* ascendance;
    cooldown_t* fire_elemental;
    cooldown_t* lava_burst;
    cooldown_t* storm_elemental;
  };

  struct gains_t : shaman_t::gains_t
  {
    gain_t* ascendance;
    gain_t* fire_elemental;
  } gain;

  struct procs_t : shaman_t::procs_t
  {
    proc_t* lava_surge;
    proc_t* wasted_lava_surge;
    proc_t* surge_during_lvb;
  };

  struct specializations_t : shaman_t::specializations_t
  {
    const spell_data_t* chain_lightning_2;  // 7.1 Chain Lightning additional 2 targets passive
    const spell_data_t* elemental_fury;     // general crit multiplier
    const spell_data_t* elemental_shaman;   // general spec multiplier
    const spell_data_t* lava_burst_2;       // 7.1 Lava Burst autocrit with FS passive
    const spell_data_t* lava_surge;
  };

  // Masteries
  struct
  {
    const spell_data_t* elemental_overload;
  } mastery;

  struct talents_t : shaman_t::talents_t
  {
    const spell_data_t* earthen_rage;
    const spell_data_t* echo_of_the_elements;

    const spell_data_t* aftershock;
    const spell_data_t* echoing_shock;

    const spell_data_t* master_of_the_elements;
    const spell_data_t* storm_elemental;
    const spell_data_t* liquid_magma_totem;

    const spell_data_t* surge_of_power;
    const spell_data_t* primal_elementalist;
    const spell_data_t* icefury;

    const spell_data_t* unlimited_power;
  } talent;

  struct misc_t : shaman_t::misc_t
  {
    const spell_data_t* maelstrom_melee_gain;
    const spell_data_t* feral_spirit;
  } spell;

  elemental_shaman_t( sim_t* sim, util::string_view name, race_e r = RACE_TAUREN ) : shaman_t( sim, name, r )
  {
    cooldown.lava_burst      = get_cooldown( "lava_burst" );
    cooldown.fire_elemental  = get_cooldown( "fire_elemental" );
    cooldown.storm_elemental = get_cooldown( "storm_elemental" );
    resource_regeneration    = regen_type::DISABLED;
  };

  ~elemental_shaman_t() override;

  void summon_fire_elemental( timespan_t duration );
  void summon_storm_elemental( timespan_t duration );

  struct earthen_rage_buff_t : public buff_t
  {
    earthen_rage_buff_t( shaman_t* p ) : buff_t( p, "earthen_rage", p->find_spell( 170377 ) )
    {
      set_refresh_behavior( buff_refresh_behavior::DURATION );
      set_tick_time_behavior( buff_tick_time_behavior::HASTED );
      set_tick_behavior( buff_tick_behavior::REFRESH );
      set_tick_callback( [ p ]( buff_t*, int, timespan_t ) {
        assert( p->action.earthen_rage );
        p->action.earthen_rage->set_target( p->recent_target );
        p->action.earthen_rage->execute();
      } );
    }
  };

  struct ele_ascendance_buff_t : public ascendance_buff_t
  {
    ele_ascendance_buff_t( elemental_shaman_t* p ) : ascendance_buff_t( p )
    {
    }
  };

  struct fire_elemental_t : public shaman_elemental_spell_t
  {
  };

  struct chained_base_t : public shaman_spell_t
  {
  };
};