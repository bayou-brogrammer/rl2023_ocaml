# Entity System Implementation

**Task ID**: TASK-001
**Status**: 🟧 In Progress
**Created**: 2023-08-02
**Priority**: High

## Description

Implement the entity system for managing game actors including the player, enemies, and items. The entity system will be responsible for creating, updating, and managing all entities in the game world.

## Related Specifications

- SPEC-004: Entity System (to be created)
- SPEC-001: [Game State Management](.cursor/specs/backend/game_state.md)

## Requirements

1. Create an entity data structure with:
   - Unique identifier
   - Position
   - Entity type (player, enemy, item)
   - Entity-specific properties (health, damage, etc.)

2. Implement entity management functions:
   - Create entities
   - Update entity state
   - Remove entities
   - Query entities by various criteria

3. Add entity interaction mechanisms:
   - Collision detection
   - Combat system
   - Item pickup/use

4. Integrate with existing systems:
   - Update Backend module to store entities
   - Connect with tilemap for positioning
   - Interface with control modes for entity control

## Acceptance Criteria

- [x] Entity data structure is defined and documented
- [x] Entity creation and management functions are implemented
- [x] Entities can be positioned and moved on the map
- [ ] Entities can interact with each other (combat, etc.)
- [x] Player entity can be controlled through input
- [x] Entity state updates correctly during game loop

## Notes

- Consider using an entity component system (ECS) approach
- Keep performance in mind for large numbers of entities
- Plan for future extensions like entity AI, equipment system, etc.
- May require updating the serialization system for save/load functionality

## Next Steps

- [ ] Defunctionalize action system: convert OO action classes to a sum-type and update Actor and turn_system to use the new Actions.execute API.
- [ ] Implement entity interaction mechanisms: collision detection, combat system, and item pickup/use.
