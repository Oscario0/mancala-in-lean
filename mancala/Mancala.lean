
structure MancalaGame :=
  (p1Entry : Array Nat)  -- Player 1's pits
  (p2Entry : Array Nat)  -- Player 2's pits
  (p1Mancala : Nat)      -- Player 1's Mancala
  (p2Mancala : Nat)      -- Player 2's Mancala
  (player_turn : Bool)   -- True if player 1's turn, False if player 2's

structure Move :=
  pitIndex : Nat
  stoneCount : Nat

def makeMove (pitIndex : Nat) (stoneCount : Nat) : Move :=
  { pitIndex := pitIndex, stoneCount := stoneCount }

def initial_board : MancalaGame :=
  { p1Entry := #[4, 4, 4, 4, 4, 4], p2Entry := #[4, 4, 4, 4, 4, 4], p1Mancala := 0, p2Mancala := 0, player_turn := true }

def is_valid_move (game : MancalaGame) (pitIndex : Nat) : Bool :=
  if game.player_turn then
    pitIndex < 6 && game.p1Entry.get! pitIndex > 0
  else
    pitIndex < 6 && game.p2Entry.get! pitIndex > 0
def move (game : MancalaGame) (pit_index : Nat) : MancalaGame :=
  let stones := if game.player_turn then game.p1Entry.get! pit_index else game.p2Entry.get! pit_index
  let new_game := if game.player_turn then
    { game with p1Entry := game.p1Entry.set! pit_index 0 }
    else
    { game with p2Entry := game.p2Entry.set! pit_index 0 }
  let rec distribute_stones (game : MancalaGame) (index : Nat) (remaining : Nat) : MancalaGame :=
    if remaining = 0 then game
    else
      let next_index := (index + 1) % 12
      let update_game := if next_index < 6 then
        if game.player_turn then { game with p1Entry := game.p1Entry.set! next_index (game.p1Entry.get! next_index + 1) }
        else { game with p2Entry := game.p2Entry.set! next_index (game.p2Entry.get! next_index + 1) }
      else if game.player_turn && next_index = 6 then { game with p1Mancala := game.p1Mancala + 1 }
      else if !game.player_turn && next_index = 6 then { game with p2Mancala := game.p2Mancala + 1 }
      else game
      distribute_stones update_game next_index (remaining - 1)
  distribute_stones new_game pit_index stones

-- Here you would ideally elaborate how the last stone and its placement affect the turn.

def game_over (game : MancalaGame) : Bool :=
  game.p1Entry.all (fun x => x = 0) || game.p2Entry.all (fun x => x = 0)

def winner (game : MancalaGame) : Option String :=
  if game_over game then
    if game.p1Mancala > game.p2Mancala then some "Player 1"
    else if game.p1Mancala < game.p2Mancala then some "Player 2"
    else some "Draw"
  else none

def play_game (game : MancalaGame) (moves : List Nat) : MancalaGame :=
  moves.foldl (fun g pit =>
    if is_valid_move g pit then move g pit else g) game

def drawBoard (game : MancalaGame) : IO Unit := do
  let p2Stones := game.p2Entry.toList.reverse.map toString
  let p1Stones := game.p1Entry.toList.map toString
  IO.println s!"Player 2's side:   | {String.intercalate " | " p2Stones} |"
  IO.println s!"P2 Mancala: {game.p2Mancala}"
  IO.println s!"Player 1's side:   | {String.intercalate " | " p1Stones} |"
  IO.println s!"P1 Mancala: {game.p1Mancala}"


partial def distribute_stones (game : MancalaGame) (index : Nat) (remainingStones : Nat) : MancalaGame :=
  if remainingStones = 0 then game
  else
    let actualIndex := index % 14 -- Assuming a total of 14 pits (6 per side + 1 Mancala each)
    if actualIndex < 6 then
      -- Update Player 1's pits or Mancala
      if game.player_turn then
        let new_p1Entry := game.p1Entry.set! actualIndex (game.p1Entry.get! actualIndex + 1)
        distribute_stones { game with p1Entry := new_p1Entry } (index + 1) (remainingStones - 1)
      else
        -- Skip Player 1's Mancala when it's Player 2's turn
        distribute_stones game (index + 1) remainingStones else
    if actualIndex == 6 && game.player_turn then
      -- Update Player 1's Mancala
      let new_p1Mancala := game.p1Mancala + 1
      distribute_stones { game with p1Mancala := new_p1Mancala } (index + 1) (remainingStones - 1)
    else if actualIndex > 6 && actualIndex < 13 then
      -- Update Player 2's pits
      if not game.player_turn then
        let new_p2Entry := game.p2Entry.set! (actualIndex - 7) (game.p2Entry.get! (actualIndex - 7) + 1)
        distribute_stones { game with p2Entry := new_p2Entry } (index + 1) (remainingStones - 1)
      else
        -- Skip Player 2's Mancala when it's Player 1's turn
        distribute_stones game (index + 1) remainingStones
    else if actualIndex == 13 && not game.player_turn then
      -- Update Player 2's Mancala
      let new_p2Mancala := game.p2Mancala + 1
      distribute_stones { game with p2Mancala := new_p2Mancala } (index + 1) (remainingStones - 1)
    else
      distribute_stones game (index + 1) remainingStones

def apply_move (game : MancalaGame) (move : Move) : MancalaGame :=
  if not (is_valid_move game move.pitIndex) then game
  else
    let stones := if game.player_turn then game.p1Entry.get! move.pitIndex else game.p2Entry.get! move.pitIndex
    -- Clear the selected pit since the stones are going to be moved.
    let cleared_game := if game.player_turn then
      { game with p1Entry := game.p1Entry.set! move.pitIndex 0 }
    else
      { game with p2Entry := game.p2Entry.set! move.pitIndex 0 }

    -- Distribute stones starting from the next pit.
    distribute_stones cleared_game (move.pitIndex + 1) stones

def main : IO Unit := do
  let game := initial_board
  IO.println "Initial game state:"
  drawBoard game

/- move creation--/
  let move1 := makeMove 3 3
  IO.println s!"Move Created for Player 2 from pit {move1.pitIndex} with {move1.stoneCount} stones"
  let game_after_move1 := apply_move game move1
  drawBoard game_after_move1

  let move2 := makeMove 1 6
  IO.println s!"Move Created for Player 2 from pit {move2.pitIndex} with {move2.stoneCount} stones"
  let game_after_move2 := apply_move game_after_move1 move2
  drawBoard game_after_move2

  /-add moves continously ...-/


  -- Final game output
  IO.println "Final game state:"
  drawBoard game_after_move2
  match winner game_after_move2 with
  | some w => IO.println s!"Winner: {w}"
  | none => IO.println "Game is still ongoing."

#eval main
