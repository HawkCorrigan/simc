#include "simulationcraft.hpp"
#include "sc_shaman.hpp"

#include "simulationcraft.hpp"

#include "sc_shaman.hpp"

namespace shaman
{

template <class Base>
struct shaman_action_t : public Base
{
private:
  using ab = Base;  // action base, eg. spell_t
public:
  using base_t = shaman_action_t<Base>;

  // Cooldown tracking
  bool track_cd_waste;
  simple_sample_data_with_min_max_t *cd_wasted_exec, *cd_wasted_cumulative;
  simple_sample_data_t* cd_wasted_iter;

  // Ghost wolf unshift
  bool unshift_ghost_wolf;

  // Maelstrom stuff
  gain_t* gain;
  // bool enable_enh_mastery_scaling;

  // bool affected_by_molten_weapon;

  // Generic procs

  shaman_action_t( const std::string& n, shaman_t* player, const spell_data_t* s = spell_data_t::nil() )
    : ab( n, player, s ),
      track_cd_waste( s->cooldown() > timespan_t::zero() || s->charge_cooldown() > timespan_t::zero() ),
      cd_wasted_exec( nullptr ),
      cd_wasted_cumulative( nullptr ),
      cd_wasted_iter( nullptr ),
      unshift_ghost_wolf( true ),
      gain( player->get_gain( s->id() > 0 ? s->name_cstr() : n ) ),
  // enable_enh_mastery_scaling( false ),
  // affected_by_molten_weapon( false )
  {
    ab::may_crit = true;

    // Auto-parse maelstrom gain from energize
    // for ( size_t i = 1; i <= ab::data().effect_count(); i++ )
    //{
    //  const spelleffect_data_t& effect = ab::data().effectN( i );
    //  if ( effect.type() != E_ENERGIZE || static_cast<power_e>( effect.misc_value1() ) != POWER_MAELSTROM )
    //  {
    //    continue;
    //  }

    //  maelstrom_gain    = effect.resource( RESOURCE_MAELSTROM );
    //  ab::energize_type = ENERGIZE_NONE;  // disable resource generation from spell data.
    //}

    if ( ab::data().affected_by( player->spec.elemental_shaman->effectN( 1 ) ) )
    {
      ab::base_dd_multiplier *= 1.0 + player->spec.elemental_shaman->effectN( 1 ).percent();
    }
    if ( ab::data().affected_by( player->spec.elemental_shaman->effectN( 2 ) ) )
    {
      ab::base_td_multiplier *= 1.0 + player->spec.elemental_shaman->effectN( 2 ).percent();
    }

    if ( ab::data().affected_by( player->spec.enhancement_shaman->effectN( 1 ) ) )
    {
      ab::base_multiplier *= 1.0 + player->spec.enhancement_shaman->effectN( 1 ).percent();
    }

    affected_by_molten_weapon =
        ab::data().affected_by_label( player->find_spell( 224125 )->effectN( 1 ).misc_value2() );
  }

  std::string full_name() const
  {
    std::string n = ab::data().name_cstr();
    return n.empty() ? ab::name_str : n;
  }

  void init() override
  {
    ab::init();

    if ( track_cd_waste )
    {
      cd_wasted_exec =
          p()->template get_data_entry<simple_sample_data_with_min_max_t, data_t>( ab::name_str, p()->cd_waste_exec );
      cd_wasted_cumulative = p()->template get_data_entry<simple_sample_data_with_min_max_t, data_t>(
          ab::name_str, p()->cd_waste_cumulative );
      cd_wasted_iter =
          p()->template get_data_entry<simple_sample_data_t, simple_data_t>( ab::name_str, p()->cd_waste_iter );
    }

    // Setup Hasted CD for Enhancement
    if ( ab::data().affected_by( p()->spec.shaman->effectN( 2 ) ) )
    {
      ab::cooldown->hasted = true;
    }

    // Setup Hasted GCD for Enhancement
    if ( ab::data().affected_by( p()->spec.shaman->effectN( 3 ) ) )
    {
      ab::gcd_type = gcd_haste_type::ATTACK_HASTE;
    }
  }

  void init_finished() override
  {
    ab::init_finished();

    if ( this->cooldown->duration > timespan_t::zero() )
    {
      p()->ability_cooldowns.push_back( this->cooldown );
    }
  }

  double composite_attack_power() const override
  {
    double m = ab::composite_attack_power();

    return m;
  }

  double recharge_multiplier( const cooldown_t& cd ) const override
  {
    double m = ab::recharge_multiplier( cd );

    m *= 1.0 / ( 1.0 + p()->buff.thundercharge->stack_value() );

    // TODO: Current presumption is self-cast, giving multiplicative effect
    m *= 1.0 / ( 1.0 + p()->buff.thundercharge->stack_value() );

    return m;
  }

  double action_multiplier() const override
  {
    double m = ab::action_multiplier();

    if ( p()->specialization() == SHAMAN_ENHANCEMENT )
    {
      if ( ( dbc::is_school( this->school, SCHOOL_FIRE ) || dbc::is_school( this->school, SCHOOL_FROST ) ||
             dbc::is_school( this->school, SCHOOL_NATURE ) ) &&
           p()->mastery.enhanced_elements->ok() )
      {
        if ( ab::data().affected_by( p()->mastery.enhanced_elements->effectN( 1 ) ) ||
             ab::data().affected_by( p()->mastery.enhanced_elements->effectN( 5 ) ) || enable_enh_mastery_scaling )
        {
          //...hopefully blizzard never makes direct and periodic scaling different from eachother in our mastery..
          m *= 1.0 + p()->cache.mastery_value();
        }
      }
    }

    if ( affected_by_molten_weapon && p()->buff.molten_weapon->check() )
    {
      m *= std::pow( p()->buff.molten_weapon->check_value(), p()->buff.molten_weapon->check() );
    }

    return m;
  }

  shaman_t* p()
  {
    return debug_cast<shaman_t*>( ab::player );
  }
  const shaman_t* p() const
  {
    return debug_cast<shaman_t*>( ab::player );
  }

  shaman_td_t* td( player_t* t ) const
  {
    return p()->get_target_data( t );
  }

  virtual double composite_maelstrom_gain_coefficient( const action_state_t* state = nullptr ) const
  {
    double m = maelstrom_gain_coefficient;

    m *= p()->composite_maelstrom_gain_coefficient( state );

    return m;
  }

  void execute() override
  {
    ab::execute();

    trigger_maelstrom_gain( ab::execute_state );
  }

  void tick( dot_t* d ) override
  {
    ab::tick( d );
  }

  void impact( action_state_t* state ) override
  {
    ab::impact( state );

    p()->trigger_stormbringer( state );
  }

  void schedule_execute( action_state_t* execute_state = nullptr ) override
  {
    if ( !ab::background && unshift_ghost_wolf )
    {
      p()->buff.ghost_wolf->expire();
    }

    ab::schedule_execute( execute_state );
  }

  void update_ready( timespan_t cd ) override
  {
    if ( cd_wasted_exec &&
         ( cd > timespan_t::zero() || ( cd <= timespan_t::zero() && ab::cooldown->duration > timespan_t::zero() ) ) &&
         ab::cooldown->current_charge == ab::cooldown->charges && ab::cooldown->last_charged > timespan_t::zero() &&
         ab::cooldown->last_charged < ab::sim->current_time() )
    {
      double time_ = ( ab::sim->current_time() - ab::cooldown->last_charged ).total_seconds();
      if ( p()->sim->debug )
      {
        p()->sim->out_debug.printf( "%s %s cooldown waste tracking waste=%.3f exec_time=%.3f", p()->name(), ab::name(),
                                    time_, ab::time_to_execute.total_seconds() );
      }
      time_ -= ab::time_to_execute.total_seconds();

      if ( time_ > 0 )
      {
        cd_wasted_exec->add( time_ );
        cd_wasted_iter->add( time_ );
      }
    }

    ab::update_ready( cd );
  }

  std::unique_ptr<expr_t> create_expression( const std::string& name ) override
  {
    if ( !util::str_compare_ci( name, "cooldown.higher_priority.min_remains" ) )
      return ab::create_expression( name );

    struct hprio_cd_min_remains_expr_t : public expr_t
    {
      action_t* action_;
      std::vector<cooldown_t*> cd_;

      // TODO: Line_cd support
      hprio_cd_min_remains_expr_t( action_t* a ) : expr_t( "min_remains" ), action_( a )
      {
        action_priority_list_t* list = a->player->get_action_priority_list( a->action_list->name_str );
        for ( auto list_action : list->foreground_action_list )
        {
          // Jump out when we reach this action
          if ( list_action == action_ )
            break;

          // Skip if this action's cooldown is the same as the list action's cooldown
          if ( list_action->cooldown == action_->cooldown )
            continue;

          // Skip actions with no cooldown
          if ( list_action->cooldown && list_action->cooldown->duration == timespan_t::zero() )
            continue;

          // Skip cooldowns that are already accounted for
          if ( std::find( cd_.begin(), cd_.end(), list_action->cooldown ) != cd_.end() )
            continue;

          // std::cout << "Appending " << list_action -> name() << " to check list" << std::endl;
          cd_.push_back( list_action->cooldown );
        }
      }

      double evaluate() override
      {
        if ( cd_.size() == 0 )
          return 0;

        timespan_t min_cd = cd_[ 0 ]->remains();
        for ( size_t i = 1, end = cd_.size(); i < end; i++ )
        {
          timespan_t remains = cd_[ i ]->remains();
          // std::cout << "cooldown.higher_priority.min_remains " << cd_[ i ] -> name_str << " remains=" <<
          // remains.total_seconds() << std::endl;
          if ( remains < min_cd )
            min_cd = remains;
        }

        // std::cout << "cooldown.higher_priority.min_remains=" << min_cd.total_seconds() << std::endl;
        return min_cd.total_seconds();
      }
    };

    return std::make_unique<hprio_cd_min_remains_expr_t>( this );
  }

  virtual void trigger_maelstrom_gain( const action_state_t* state )
  {
    if ( maelstrom_gain == 0 )
    {
      return;
    }

    double g = maelstrom_gain;
    g *= composite_maelstrom_gain_coefficient( state );
    // TODO: Some sort of selector whether it's per cast or per target. Per target is the "default".
    g *= state->n_targets;
    ab::player->resource_gain( RESOURCE_MAELSTROM, g, gain, this );
  }
};

template <class Base>
struct shaman_spell_base_t : public shaman_action_t<Base>
{
private:
  using ab = shaman_action_t<Base>;

public:
  using base_t = shaman_spell_base_t<Base>;

  shaman_spell_base_t( const std::string& n, shaman_t* player, const spell_data_t* s = spell_data_t::nil() )
    : ab( n, player, s )
  {
  }

  void execute() override
  {
    ab::execute();

    // for benefit tracking purpose
    ab::p()->buff.spiritwalkers_grace->up();

    if ( ab::p()->talent.aftershock->ok() && ab::current_resource() == RESOURCE_MAELSTROM &&
         ab::last_resource_cost > 0 && ab::rng().roll( ab::p()->talent.aftershock->effectN( 1 ).percent() ) )
    {
      ab::p()->trigger_maelstrom_gain( ab::last_resource_cost, ab::p()->gain.aftershock );
    }
  }
};

struct shaman_elemental_spell_t : public shaman_spell_base_t<spell_t>
{
  action_t* overload;

public:
  bool affected_by_master_of_the_elements = false;
  bool affected_by_stormkeeper            = false;

  shaman_elemental_spell_t(const std::string& token, shaman_t* p, const spell_data_t* s = spell_data_t::nil(),
                            const std::string& options = std::string() )
    : base_t( token, p, s ), overload(nullptr)
  {
    parse_options( options );

    if ( data().affected_by( p->spec.elemental_fury->effectN( 1 ) ) )
    {
      crit_bonus_multiplier *= 1.0 + p->spec.elemental_fury->effectN( 1 ).percent();
    }

    if ( data().affected_by( p->find_spell( 260734 )->effectN( 1 ) ) )
    {
      affected_by_master_of_the_elements = true;
    }

    if ( data().affected_by( p->find_spell( 191634 )->effectN( 1 ) ) )
    {
      affected_by_stormkeeper = true;
    }
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

    double composite_spell_power() const override
  {
    double sp = base_t::composite_spell_power();

    return sp;
  }

  void execute() override
  {
    base_t::execute();

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

    base_t::schedule_travel( s );
  }

  bool usable_moving() const override
  {
    if ( p()->buff.spiritwalkers_grace->check() || execute_time() == timespan_t::zero() )
      return true;

    return base_t::usable_moving();
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

  void impact( action_state_t* state ) override
  {
    base_t::impact( state );
  }
};



struct elemental_shaman_td_t : public shaman_td_t
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

  elemental_shaman_td_t( player_t* target, elemental_shaman_t* p );
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

  struct
  {
  } covenants;

  struct
  {
    //conduit_data_t
  } conduits;

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
};

struct shaman_totem_pet_t : public pet_t
{
  // Pulse related functionality
  totem_pulse_action_t* pulse_action;
  event_t* pulse_event;
  timespan_t pulse_amplitude;

  // Summon related functionality
  std::string pet_name;
  pet_t* summon_pet;

  shaman_totem_pet_t( shaman_t* p, const std::string& n )
    : pet_t( p->sim, p, n, true ),
      pulse_action( nullptr ),
      pulse_event( nullptr ),
      pulse_amplitude( timespan_t::zero() ),
      summon_pet( nullptr )
  {
    resource_regeneration = regen_type::DISABLED;
  }

  void summon( timespan_t = timespan_t::zero() ) override;
  void dismiss( bool expired = false ) override;

  void init_finished() override
  {
    if ( !pet_name.empty() )
    {
      summon_pet = owner->find_pet( pet_name );
    }

    pet_t::init_finished();
  }

  shaman_t* o()
  {
    return debug_cast<shaman_t*>( owner );
  }

  /*
  //  Code to make a totem double dip on player multipliers.
  //  As of 7.3.5 this is no longer needed for Liquid Magma Totem (Elemental)
  virtual double composite_player_multiplier( school_e school ) const override
  { return owner -> cache.player_multiplier( school ); }
  //*/

  double composite_spell_hit() const override
  {
    return owner->cache.spell_hit();
  }

  double composite_spell_crit_chance() const override
  {
    return owner->cache.spell_crit_chance();
  }

  double composite_spell_power( school_e school ) const override
  {
    return owner->cache.spell_power( school );
  }

  double composite_spell_power_multiplier() const override
  {
    return owner->composite_spell_power_multiplier();
  }

  std::unique_ptr<expr_t> create_expression( util::string_view name ) override
  {
    if ( util::str_compare_ci( name, "duration" ) )
      return make_ref_expr( name, duration );

    return pet_t::create_expression( name );
  }
};

struct totem_pulse_action_t : public spell_t
{
  bool hasted_pulse;
  double pulse_multiplier;
  shaman_totem_pet_t* totem;

  totem_pulse_action_t( const std::string& token, shaman_totem_pet_t* p, const spell_data_t* s )
    : spell_t( token, p, s ), hasted_pulse( false ), pulse_multiplier( 1.0 ), totem( p )
  {
    may_crit = harmful = background = true;
    callbacks                       = false;

    crit_bonus_multiplier *= 1.0 + totem->o()->spec.elemental_fury->effectN( 1 ).percent();
  }

  shaman_t* o() const
  {
    return debug_cast<shaman_t*>( player->cast_pet()->owner );
  }

  shaman_td_t* td( player_t* target ) const
  {
    return o()->get_target_data( target );
  }

  double action_multiplier() const override
  {
    double m = spell_t::action_multiplier();

    m *= pulse_multiplier;

    return m;
  }

  void init() override
  {
    spell_t::init();

    // Hacky, but constructor wont work.
    crit_multiplier *= util::crit_multiplier( totem->o()->meta_gem );
  }

  void reset() override
  {
    spell_t::reset();
    pulse_multiplier = 1.0;
  }
};

struct totem_pulse_event_t : public event_t
{
  shaman_totem_pet_t* totem;
  timespan_t real_amplitude;

  totem_pulse_event_t( shaman_totem_pet_t& t, timespan_t amplitude )
    : event_t( t ), totem( &t ), real_amplitude( amplitude )
  {
    if ( totem->pulse_action->hasted_pulse )
      real_amplitude *= totem->cache.spell_speed();

    schedule( real_amplitude );
  }
  const char* name() const override
  {
    return "totem_pulse";
  }
  void execute() override
  {
    if ( totem->pulse_action )
      totem->pulse_action->execute();

    totem->pulse_event = make_event<totem_pulse_event_t>( sim(), *totem, totem->pulse_amplitude );
  }
};

void shaman_totem_pet_t::summon( timespan_t duration )
{
  pet_t::summon( duration );

  if ( pulse_action )
  {
    pulse_action->pulse_multiplier = 1.0;
    pulse_event                    = make_event<totem_pulse_event_t>( *sim, *this, pulse_amplitude );
  }

  if ( summon_pet )
    summon_pet->summon();
}

void shaman_totem_pet_t::dismiss( bool expired )
{
  // Disable last (partial) tick on dismiss, as it seems not to happen in game atm
  if ( pulse_action && pulse_event && expiration && expiration->remains() == timespan_t::zero() )
  {
    if ( pulse_event->remains() > timespan_t::zero() )
      pulse_action->pulse_multiplier =
          pulse_event->remains() / debug_cast<totem_pulse_event_t*>( pulse_event )->real_amplitude;
    pulse_action->execute();
  }

  event_t::cancel( pulse_event );

  if ( summon_pet )
    summon_pet->dismiss();

  pet_t::dismiss( expired );
}

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
    fire_elemental_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "fire_elemental", player, player->find_specialization_spell( "Fire Elemental" ), options_str )
    {
      harmful = may_crit = false;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      p()->summon_fire_elemental( p()->spell.fire_elemental->duration(), false );
    }

    bool ready() override
    {
      if ( p()->talent.storm_elemental->ok() )
      {
        return false;
      }

      return shaman_elemental_spell_t::ready();
    }
  };

  struct storm_elemental_t : public shaman_elemental_spell_t
  {
    storm_elemental_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "storm_elemental", player, player->talent.storm_elemental, options_str )
    {
      harmful = may_crit = false;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      p()->summon_storm_elemental( p()->spell.storm_elemental->duration(), false );
    }
  };

  struct earthen_rage_spell_t : public shaman_elemental_spell_t
  {
    earthen_rage_spell_t( shaman_t* p ) : shaman_elemental_spell_t( "earthen_rage", p, p->find_spell( 170379 ) )
    {
      background = proc = true;
    }
  };

  struct lava_burst_overload_t : public elemental_overload_spell_t
  {
    unsigned impact_flags;

    lava_burst_overload_t( shaman_t* player )
      : elemental_overload_spell_t( player, "lava_burst_overload", player->find_spell( 77451 ) ), impact_flags()
    {
      spell_power_mod.direct = player->find_spell( 285466 )->effectN( 1 ).sp_coeff();
    }

    void init() override
    {
      elemental_overload_spell_t::init();

      std::swap( snapshot_flags, impact_flags );
    }

    void snapshot_impact_state( action_state_t* s, result_amount_type rt )
    {
      snapshot_internal( s, impact_flags, rt );
    }

    double calculate_direct_amount( action_state_t* /* s */ ) const override
    {
      // Don't do any extra work, this result won't be used.
      return 0.0;
    }

    result_e calculate_result( action_state_t* /* s */ ) const override
    {
      // Don't do any extra work, this result won't be used.
      return RESULT_NONE;
    }

    void impact( action_state_t* s ) override
    {
      // Re-call functions here, before the impact call to do the damage calculations as we impact.
      snapshot_impact_state( s, amount_type( s ) );

      s->result        = elemental_overload_spell_t::calculate_result( s );
      s->result_amount = elemental_overload_spell_t::calculate_direct_amount( s );

      elemental_overload_spell_t::impact( s );
    }

    double action_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_multiplier();

      if ( p()->buff.ascendance->up() )
      {
        m *= 1.0 + p()->cache.spell_crit_chance();
      }

      return m;
    }

    double bonus_da( const action_state_t* s ) const override
    {
      double b = shaman_elemental_spell_t::bonus_da( s );

      return b;
    }

    double composite_target_crit_chance( player_t* t ) const override
    {
      double m = shaman_elemental_spell_t::composite_target_crit_chance( t );

      if ( p()->spec.lava_burst_2->ok() && td( target )->dot.flame_shock->is_ticking() )
      {
        // hardcoded because I didn't find it in spelldata yet
        m = 1.0;
      }

      return m;
    }
  };

  struct lava_burst_t : public shaman_elemental_spell_t
  {
    unsigned impact_flags;

    lava_burst_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "lava_burst", player, player->find_specialization_spell( "Lava Burst" ),
                                  options_str ),
        impact_flags()
    {
      if ( player->mastery.elemental_overload->ok() )
      {
        overload = new lava_burst_overload_t( player );
        add_child( overload );
      }

      spell_power_mod.direct = player->find_spell( 285452 )->effectN( 1 ).sp_coeff();
    }

    void init() override
    {
      shaman_elemental_spell_t::init();

      std::swap( snapshot_flags, impact_flags );

      // Elemental and Restoration gain a second Lava Burst charge via Echo of the Elements
      if ( p()->talent.echo_of_the_elements->ok() )
      {
        cooldown->charges = (int)data().charges() + (int)p()->talent.echo_of_the_elements->effectN( 2 ).base_value();
      }
    }

    void snapshot_impact_state( action_state_t* s, result_amount_type rt )
    {
      snapshot_internal( s, impact_flags, rt );
    }

    double calculate_direct_amount( action_state_t* /* s */ ) const override
    {
      // Don't do any extra work, this result won't be used.
      return 0.0;
    }

    result_e calculate_result( action_state_t* /* s */ ) const override
    {
      // Don't do any extra work, this result won't be used.
      return RESULT_NONE;
    }

    void impact( action_state_t* s ) override
    {
      // Re-call functions here, before the impact call to do the damage calculations as we impact.
      snapshot_impact_state( s, amount_type( s ) );

      s->result        = shaman_elemental_spell_t::calculate_result( s );
      s->result_amount = shaman_elemental_spell_t::calculate_direct_amount( s );

      shaman_elemental_spell_t::impact( s );

      if ( result_is_hit( s->result ) )
      {
        if ( p()->buff.surge_of_power->up() )
        {
          p()->cooldown.fire_elemental->adjust( -1.0 * p()->talent.surge_of_power->effectN( 1 ).time_value() );
          p()->cooldown.storm_elemental->adjust( -1.0 * p()->talent.surge_of_power->effectN( 1 ).time_value() );
          p()->buff.surge_of_power->decrement();
        }
      }
    }

    double bonus_da( const action_state_t* s ) const override
    {
      double b = shaman_elemental_spell_t::bonus_da( s );

      return b;
    }

    double action_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_multiplier();

      if ( p()->buff.ascendance->up() )
      {
        m *= 1.0 + p()->cache.spell_crit_chance();
      }

      return m;
    }

    double composite_target_crit_chance( player_t* t ) const override
    {
      double m = shaman_elemental_spell_t::composite_target_crit_chance( t );

      if ( p()->spec.lava_burst_2->ok() && td( target )->dot.flame_shock->is_ticking() )
      {
        // hardcoded because I didn't find it in spell data yet
        m = 1.0;
      }

      return m;
    }

    void update_ready( timespan_t /* cd_duration */ ) override
    {
      timespan_t d = cooldown->duration;

      if ( p()->buff.ascendance->up() )
      {
        d = timespan_t::zero();
      }

      // Lava Surge has procced during the cast of Lava Burst, the cooldown
      // reset is deferred to the finished cast, instead of "eating" it.
      if ( p()->lava_surge_during_lvb )
      {
        d                      = timespan_t::zero();
        cooldown->last_charged = sim->current_time();
      }

      shaman_elemental_spell_t::update_ready( d );
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      if ( p()->talent.master_of_the_elements->ok() )
      {
        p()->buff.master_of_the_elements->trigger();
      }

      // Lava Surge buff does not get eaten, if the Lava Surge proc happened
      // during the Lava Burst cast
      if ( !p()->lava_surge_during_lvb && p()->buff.lava_surge->check() )
        p()->buff.lava_surge->expire();

      p()->lava_surge_during_lvb = false;
    }

    timespan_t execute_time() const override
    {
      if ( p()->buff.lava_surge->up() )
      {
        return timespan_t::zero();
      }

      return shaman_elemental_spell_t::execute_time();
    }
  };

  struct lightning_bolt_overload_t : public elemental_overload_spell_t
  {
    lightning_bolt_overload_t( shaman_t* p )
      : elemental_overload_spell_t( p, "lightning_bolt_overload", p->find_spell( 45284 ) )
    {
      affected_by_master_of_the_elements = true;
    }

    double composite_target_multiplier( player_t* target ) const override
    {
      auto m = shaman_elemental_spell_t::composite_target_multiplier( target );
      return m;
    }

    double action_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_multiplier();
      if ( p()->buff.stormkeeper->up() )
      {
        m *= 1.0 + p()->talent.stormkeeper->effectN( 2 ).percent();
      }
      return m;
    }

    void impact( action_state_t* state ) override
    {
      elemental_overload_spell_t::impact( state );
    }
  };

  struct lightning_bolt_t : public shaman_elemental_spell_t
  {
    double m_overcharge;

    lightning_bolt_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "lightning_bolt", player, player->find_class_spell( "Lightning Bolt" ), options_str ),
        m_overcharge( 0 )
    {
      if ( player->specialization() == SHAMAN_ELEMENTAL )
      {
        affected_by_master_of_the_elements = true;
      }

      if ( player->mastery.elemental_overload->ok() )
      {
        overload = new lightning_bolt_overload_t( player );
        add_child( overload );
      }
    }

    double overload_chance( const action_state_t* s ) const override
    {
      double chance = shaman_elemental_spell_t::overload_chance( s );

      /*
      if ( p()->buff.stormkeeper->check() )
      {
        chance = 1.0;
      }*/

      return chance;
    }

    /* Number of guaranteed overloads */
    size_t n_overloads( const action_state_t* t ) const override
    {
      size_t n = shaman_elemental_spell_t::n_overloads( t );
      // Surge of Power is an addition to the base overload chance
      if ( p()->buff.surge_of_power->up() )
      {
        // roll one 100 sided dice once.
        //  2% chance to get 3 overloads,
        // 18% chance to get 2 overloads,
        // 80% chance to get 1 overload
        double roll = rng().real();
        if ( roll >= 0.98 )
        {
          n += 3u;
        }
        else if ( roll >= 0.8 )
        {
          n += 2u;
        }
        else
        {
          n += 1u;
        }
      }

      return n;
    }

    /* Number of potential overloads */
    size_t n_overload_chances( const action_state_t* t ) const override
    {
      return shaman_elemental_spell_t::n_overload_chances( t );
    }

    double composite_target_multiplier( player_t* target ) const override
    {
      auto m = shaman_elemental_spell_t::composite_target_multiplier( target );
      return m;
    }

    double action_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_multiplier();
      if ( p()->buff.stormkeeper->up() )
      {
        m *= 1.0 + p()->talent.stormkeeper->effectN( 2 ).percent();
      }
      return m;
    }

    double spell_direct_power_coefficient( const action_state_t* /* state */ ) const override
    {
      return spell_power_mod.direct * ( 1.0 + m_overcharge * cost() );
    }

    timespan_t execute_time() const override
    {
      if ( p()->buff.stormkeeper->up() )
      {
        return timespan_t::zero();
      }

      return shaman_elemental_spell_t::execute_time() * ( 1.0 + p()->buff.wind_gust->stack_value() );
    }

    timespan_t gcd() const override
    {
      timespan_t t = shaman_elemental_spell_t::gcd();
      t *= 1.0 + p()->buff.wind_gust->stack_value();

      // testing shows the min GCD is 0.5 sec
      if ( t < timespan_t::from_millis( 500 ) )
      {
        t = timespan_t::from_millis( 500 );
      }
      return t;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      p()->buff.stormkeeper->decrement();

      p()->buff.surge_of_power->decrement();

      // Storm Elemental Wind Gust passive buff trigger
      if ( p()->talent.storm_elemental->ok() )
      {
        if ( p()->talent.primal_elementalist->ok() && p()->pet.pet_storm_elemental &&
             !p()->pet.pet_storm_elemental->is_sleeping() )
        {
          p()->buff.wind_gust->trigger();
        }
        else if ( !p()->talent.primal_elementalist->ok() && p()->pet.guardian_storm_elemental &&
                  !p()->pet.guardian_storm_elemental->is_sleeping() )
        {
          p()->buff.wind_gust->trigger();
        }
      }
    }

    void reset_swing_timers()
    {
      if ( player->main_hand_attack && player->main_hand_attack->execute_event )
      {
        event_t::cancel( player->main_hand_attack->execute_event );
        player->main_hand_attack->schedule_execute();
      }

      if ( player->off_hand_attack && player->off_hand_attack->execute_event )
      {
        event_t::cancel( player->off_hand_attack->execute_event );
        player->off_hand_attack->schedule_execute();
      }
    }
  };

  struct elemental_overload_spell_t : public shaman_elemental_spell_t
  {
    elemental_overload_spell_t( shaman_t* p, const std::string& name, const spell_data_t* s )
      : shaman_elemental_spell_t( name, p, s )
    {
      base_execute_time = timespan_t::zero();
      background        = true;
      callbacks         = false;

      base_multiplier *= p->mastery.elemental_overload->effectN( 2 ).percent();
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      if ( p()->talent.unlimited_power->ok() )
      {
        p()->buff.unlimited_power->trigger();
      }
    }
  };

  struct thunderstorm_t : public shaman_elemental_spell_t
  {
    thunderstorm_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "thunderstorm", player, player->find_specialization_spell( "Thunderstorm" ),
                                  options_str )
    {
      aoe = -1;
    }
  };

  struct earthquake_damage_t : public shaman_elemental_spell_t
  {
    double kb_chance;

    earthquake_damage_t( shaman_t* player )
      : shaman_elemental_spell_t( "earthquake_", player, player->find_spell( 77478 ) ),
        kb_chance( data().effectN( 2 ).percent() )
    {
      aoe        = -1;
      ground_aoe = background = true;
      school                  = SCHOOL_PHYSICAL;
      spell_power_mod.direct  = 0.2875;  // still cool to hardcode the SP% into tooltip
    }

    double target_armor( player_t* ) const override
    {
      return 0;
    }

    double composite_persistent_multiplier( const action_state_t* state ) const override
    {
      double m = shaman_elemental_spell_t::composite_persistent_multiplier( state );

      m *= 1.0 + p()->buff.master_of_the_elements->value();

      return m;
    }

    void impact( action_state_t* state ) override
    {
      shaman_elemental_spell_t::impact( state );
    }
  };

  struct earthquake_t : public shaman_elemental_spell_t
  {
    earthquake_damage_t* rumble;

    earthquake_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "earthquake", player, player->find_specialization_spell( "Earthquake" ),
                                  options_str ),
        rumble( new earthquake_damage_t( player ) )
    {
      dot_duration = timespan_t::zero();  // The periodic effect is handled by ground_aoe_event_t
      add_child( rumble );
    }

    double cost() const override
    {
      double d = shaman_elemental_spell_t::cost();
      return d;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      make_event<ground_aoe_event_t>(
          *sim, p(),
          ground_aoe_params_t().target( execute_state->target ).duration( data().duration() ).action( rumble ) );

      // Note, needs to be decremented after ground_aoe_event_t is created so that the rumble gets the
      // buff multiplier as persistent.
      p()->buff.master_of_the_elements->expire();
    }
  };

  struct earth_shock_t : public shaman_elemental_spell_t
  {

    earth_shock_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "earth_shock", player, player->find_specialization_spell( "Earth Shock" ),
                                  options_str )
    {

      affected_by_master_of_the_elements = true;
    }

    double bonus_da( const action_state_t* s ) const override
    {
      double b = shaman_elemental_spell_t::bonus_da( s );

      return b;
    }

    double action_multiplier() const override
    {
      auto m = shaman_elemental_spell_t::action_multiplier();

      return m;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      if ( p()->talent.surge_of_power->ok() )
      {
        p()->buff.surge_of_power->trigger();
      }
    }

    void impact( action_state_t* state ) override
    {
      shaman_elemental_spell_t::impact( state );
    }
  };

  // Flame Shock Spell ========================================================

  struct flame_shock_t : public shaman_elemental_spell_t
  {
    const spell_data_t* elemental_resource;
    const spell_data_t* t20_4pc_bonus;

    flame_shock_t( shaman_t* player, const std::string& options_str = std::string() )
      : shaman_elemental_spell_t( "flame_shock", player, player->find_class_spell( "Flame Shock" ), options_str ),
        //spreader( player->talent.surge_of_power->ok() ? new flame_shock_spreader_t( player ) : nullptr ),
        elemental_resource( player->find_spell( 263819 ) )
    {
      tick_may_crit  = true;
      track_cd_waste = false;
    }

    double composite_crit_chance() const override
    {
      double m = shaman_elemental_spell_t::composite_crit_chance();

      return m;
    }

    double action_ta_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_ta_multiplier();

      return m;
    }

    void tick( dot_t* d ) override
    {
      shaman_elemental_spell_t::tick( d );

      double proc_chance = p()->spec.lava_surge->proc_chance();

      // proc chance suddenly bacame 100% and the actual chance became effectN 1
      proc_chance = p()->spec.lava_surge->effectN( 1 ).percent();

      if ( p()->spec.restoration_shaman->ok() )
      {
        proc_chance += p()->spec.restoration_shaman->effectN( 8 ).percent();
      }

      if ( rng().roll( proc_chance ) )
      {
        if ( p()->buff.lava_surge->check() )
          p()->proc.wasted_lava_surge->occur();

        p()->proc.lava_surge->occur();
        if ( !p()->executing || p()->executing->id != 51505 )
          p()->cooldown.lava_burst->reset( true );
        else
        {
          p()->proc.surge_during_lvb->occur();
          p()->lava_surge_during_lvb = true;
        }

        p()->buff.lava_surge->trigger();
      }

      // Fire Elemental passive effect (MS generation on FS tick)
      if ( !p()->talent.storm_elemental->ok() )
      {
        if ( p()->talent.primal_elementalist->ok() && p()->pet.pet_fire_elemental &&
             !p()->pet.pet_fire_elemental->is_sleeping() )
        {
          p()->trigger_maelstrom_gain( elemental_resource->effectN( 1 ).base_value(), p()->gain.fire_elemental );
        }
        else if ( !p()->talent.primal_elementalist->ok() && p()->pet.guardian_fire_elemental &&
                  !p()->pet.guardian_fire_elemental->is_sleeping() )
        {
          p()->trigger_maelstrom_gain( elemental_resource->effectN( 1 ).base_value(), p()->gain.fire_elemental );
        }
      }
    }

    void impact( action_state_t* state ) override
    {
      shaman_elemental_spell_t::impact( state );
      if ( p()->buff.surge_of_power->up() && sim->target_non_sleeping_list.size() > 1 )
      {
        //spreader->target = state->target;
        //spreader->execute();
      }
      p()->buff.surge_of_power->expire();
    }
  };

  // Frost Shock Spell ========================================================

  struct frost_shock_t : public shaman_elemental_spell_t
  {
    frost_shock_t( shaman_t* player, const std::string& options_str )
      : shaman_elemental_spell_t( "frost_shock", player, player->find_class_spell( "Frost Shock" ), options_str )

    {
      affected_by_master_of_the_elements = true;
    }

    double action_multiplier() const override
    {
      double m = shaman_elemental_spell_t::action_multiplier();

      m *= 1.0 + p()->buff.icefury->value();

      return m;
    }

    void execute() override
    {
      shaman_elemental_spell_t::execute();

      p()->buff.icefury->decrement();
    }

    void impact( action_state_t* state ) override
    {
      shaman_elemental_spell_t::impact( state );
    }
  };

  // Liquid Magma totem =======================================================

  struct liquid_magma_globule_t : public spell_t
  {
    liquid_magma_globule_t( shaman_totem_pet_t* p ) : spell_t( "liquid_magma", p, p->find_spell( 192231 ) )
    {
      aoe        = -1;
      background = may_crit = true;
      callbacks             = false;

      if ( p->o()->spec.elemental_fury->ok() )
      {
        crit_bonus_multiplier *= 1.0 + p->o()->spec.elemental_fury->effectN( 1 ).percent();
      }
    }
  };

  struct liquid_magma_totem_pulse_t : public totem_pulse_action_t
  {
    liquid_magma_globule_t* globule;

    liquid_magma_totem_pulse_t( shaman_totem_pet_t* totem )
      : totem_pulse_action_t( "liquid_magma_driver", totem, totem->find_spell( 192226 ) ),
        globule( new liquid_magma_globule_t( totem ) )
    {
      // TODO: "Random enemies" implicates number of targets
      aoe          = 1;
      hasted_pulse = quiet = dual = true;
      dot_duration                = timespan_t::zero();
    }

    void impact( action_state_t* state ) override
    {
      totem_pulse_action_t::impact( state );

      globule->set_target( state->target );
      globule->schedule_execute();
    }
  };

  struct liquid_magma_totem_t : public shaman_totem_pet_t
  {
    liquid_magma_totem_t( shaman_t* owner ) : shaman_totem_pet_t( owner, "liquid_magma_totem" )
    {
      pulse_amplitude = owner->find_spell( 192226 )->effectN( 1 ).period();
    }

    void init_spells() override
    {
      shaman_totem_pet_t::init_spells();

      pulse_action = new liquid_magma_totem_pulse_t( this );
    }
  };

  void elemental_shaman_t::summon_fire_elemental( timespan_t duration )
  {
    if ( talent.storm_elemental->ok() )
    {
      return;
    }

    if ( talent.primal_elementalist->ok() )
    {
      if ( pet.pet_fire_elemental->is_sleeping() )
      {
        pet.pet_fire_elemental->summon( duration );
        pet.pet_fire_elemental->get_cooldown( "meteor" )->reset( false );
      }
      else
      {
        auto new_duration = pet.pet_fire_elemental->expiration->remains();
        new_duration += duration;
        pet.pet_fire_elemental->expiration->reschedule( new_duration );
      }
    }
    else
    {
      if ( pet.guardian_fire_elemental->is_sleeping() )
      {
        pet.guardian_fire_elemental->summon( duration );
      }
      else
      {
        auto new_duration = pet.guardian_fire_elemental->expiration->remains();
        new_duration += duration;
        pet.guardian_fire_elemental->expiration->reschedule( new_duration );
      }
    }
  }

  void elemental_shaman_t::summon_storm_elemental( timespan_t duration )
  {
    if ( !talent.storm_elemental->ok() )
    {
      return;
    }

    if ( talent.primal_elementalist->ok() )
    {
      if ( pet.pet_storm_elemental->is_sleeping() )
      {
        pet.pet_storm_elemental->summon( duration );
        pet.pet_storm_elemental->get_cooldown( "eye_of_the_storm" )->reset( false );
      }
      else
      {
        auto new_duration = pet.pet_storm_elemental->expiration->remains();
        new_duration += duration;
        pet.pet_storm_elemental->expiration->reschedule( new_duration );
      }
    }
    else
    {
      if ( pet.guardian_storm_elemental->is_sleeping() )
      {
        pet.guardian_storm_elemental->summon( duration );
      }
      else
      {
        auto new_duration = pet.guardian_storm_elemental->expiration->remains();
        new_duration += duration;
        pet.guardian_storm_elemental->expiration->reschedule( new_duration );
      }
    }
  }

  action_t* shaman_t::create_action( util::string_view name, const std::string& options_str )
  {
    // elemental
    if ( name == "chain_lightning" )
      return new chain_lightning_t( this, options_str );
    if ( name == "earth_shock" )
      return new earth_shock_t( this, options_str );
    if ( name == "earthquake" )
      return new earthquake_t( this, options_str );
    if ( name == "elemental_blast" )
      return new elemental_blast_t( this, options_str );
    if ( name == "fire_elemental" )
      return new fire_elemental_t( this, options_str );
    if ( name == "flame_shock" )
      return new flame_shock_t( this, options_str );
    if ( name == "frost_shock" )
      return new frost_shock_t( this, options_str );
    if ( name == "icefury" )
      return new icefury_t( this, options_str );
    if ( name == "lava_beam" )
      return new lava_beam_t( this, options_str );
    if ( name == "lava_burst" )
      return new lava_burst_t( this, options_str );
    if ( name == "liquid_magma_totem" )
      return new shaman_totem_t( "liquid_magma_totem", this, options_str, talent.liquid_magma_totem );
    if ( name == "storm_elemental" )
      return new storm_elemental_t( this, options_str );
    if ( name == "stormkeeper" )
      return new stormkeeper_elemental_t( this, options_str );
    if ( name == "thunderstorm" )
      return new thunderstorm_t( this, options_str );
    if ( name == "totem_mastery" )
      return new totem_mastery_t( this, options_str );

    return shaman_t::create_action( name, options_str );
  }

  }