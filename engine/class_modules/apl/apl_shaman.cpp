#include "class_modules/apl/apl_shaman.hpp"

#include "player/action_priority_list.hpp"
#include "player/player.hpp"

namespace shaman_apl
{

std::string flask_elemental( const player_t* p )
{
  return "disabled";
}

std::string food_elemental( const player_t* p )
{
  return "disabled";
}

std::string potion_elemental( const player_t* p )
{
  return "disabled";
}

std::string temporary_enchant_elemental( const player_t* p )
{
  return "disabled";
}

std::string rune( const player_t* p )
{
  return "disabled";
}

//elemental_apl_start
void elemental( player_t* p )
{
  action_priority_list_t* default_ = p->get_action_priority_list( "default" );
  action_priority_list_t* precombat = p->get_action_priority_list( "precombat" );
  action_priority_list_t* aoe = p->get_action_priority_list( "aoe" );
  action_priority_list_t* single_target = p->get_action_priority_list( "single_target" );


  precombat->add_action( "lightning_shield" );
  precombat->add_action( "snapshot_stats", "Snapshot raid buffed stats before combat begins and pre-potting is done." );

  default_->add_action( "run_action_list,name=aoe,if=spell_targets.chain_lightning>=2" );
  default_->add_action( "run_action_list,name=single_target" );

  aoe->add_action( "chain_lightning" );
  single_target->add_action( "flame_shock,if=time<1" );
  single_target->add_action( "lightning_bolt" );
}
//elemental_apl_end

//elemental_ptr_apl_start
void elemental_ptr( player_t* p )
{
  action_priority_list_t* default_ = p->get_action_priority_list( "default" );
  action_priority_list_t* precombat = p->get_action_priority_list( "precombat" );
  action_priority_list_t* aoe = p->get_action_priority_list( "aoe" );
  action_priority_list_t* single_target = p->get_action_priority_list( "single_target" );

  precombat->add_action( "snapshot_stats", "Snapshot raid buffed stats before combat begins and pre-potting is done." );

  default_->add_action( "run_action_list,name=aoe,if=spell_targets.chain_lightning>=2" );
  default_->add_action( "run_action_list,name=single_target" );

  aoe->add_action( "chain_lightning" );
  single_target->add_action( "lightning_bolt" );
}
//elemental_ptr_apl_end

} //namespace shaman_apl
