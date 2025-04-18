# Multi-Level Map Implementation

**Task ID**: TASK-002
**Status**: ✅ Done
**Created**: 2023-08-03
**Priority**: High

## Description

Implement multi-level map generation and seamless transitions between levels, ensuring map caching and UI integration when the player uses stairs.

## Requirements

1. Ensure `MultiLevelState.maps` caches each generated `Map.Tilemap.t` for all levels.
2. In `load_next_level` and `load_prev_level`, generate and cache maps on demand if missing.
3. Update `game_state.backend.map` on level change.
4. Update UI to render the new map and reposition the player at `player_start` on level load.
5. Handle edge cases (first/last level without generating or transitioning incorrectly).

## Acceptance Criteria

- [x] Maps for new levels are generated and stored in `MultiLevelState.maps`.
- [x] `load_next_level` and `load_prev_level` correctly update the current map in the backend.
- [x] Rendering switches to the new level map smoothly when using stairs up/down.
- [x] Player start position aligns with `tilemap.player_start` on each loaded level.
- [x] Unit tests cover multi-level loading functions and their edge conditions.
