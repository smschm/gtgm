XML-RPC Protocol for *Ghost Towns*
======================================

The architecture of this game is one entity responsible for organizing matches
between different bots, managing game state, informing bots of this game state,
and sending appropriate requests to them asking for next actions.
Bots playing the game are expected to implement an XML-RPC *server* to respond
to these requests, and the central entity will act as an XML-RPC *client*.
To curb the natural tendency to call the central entity a "server" and the bots
"clients", we will refer to the central entity as the GM and the bots as PCs.

The XML-RPC functions that the GM will call and the PCs expected response are:

### `startGame`

Signals the PC that the GM would like to begin a game.
The PC has the opportunity to decline the game in its response.

**Parameters:** None

**Response:** A boolean; true if the PC would like to play, false otherwise.

### `initialize`

Signals the PC that the game is beginning.

**Parameters:**

* `game_id` (int) The ID of the game.
* `opponent_id` (int) The ID of the opponent.
* `player_num` (int) 0 if you are the first player, 1 if you are second.
* `hand` (array) An array of cards in hand. The order is significant,
  as a card's index will be used to identify it later.
  A card's datatype is a struct with fields:
  * `rank` (int) 0 if an investment card, 2-10 if an expedition card.
  * `suit` (int) The suit (color) of the card, from 0-4.

**Response:** A boolean; true to confirm this, but the GM will probably ignore it.

### `getPlay`

Requests a location to play a card to.

**Parameters:**

* `hand` (array) Array of cards in hand.
* `discards` (array of array) Array of discard piles.
* `expos0` (array of array) Expeditions for player 0.
* `expos1` (array of array) Expeditions for player 1.

**Response:** A struct containing fields named:

* `card_ix` (int) The index of the card to play.
* `play_to` (int) Equal to 0 if playing on the discard pile;
  1 if on an expedition. Which discard or expedition is inferred from the suit.
  **Note:** Playing on an expedition when it is not allowed will silently
  play it on the discard instead.
* `draw_from` (int) Equal to -1 if drawing from the deck; if 0 or greater,
  draw from the discard pile corresponding to this suit.
  **Note:** Drawing from a discard when it is empty will silently
  draw from the deck.

### `opponentPlay`

Gives information about an opponent play.

**Parameters:**

* `card_played` (card) Card played.
* `play_to` (int) Where the card was played; again 0 means discard,
  1 on an expedition.
* `draw_from` (int) Where the card was drawn from; -1 from the deck,
  0-4 for the corresponding suit's discard pile.
  
**Response:** A boolean. True confirms the card recieved. A false can signal that
the PC protests their card, but the GM will ignore this.

### `gameEnd`

Game is over. Scores are reported.

**Parameters:**

* `score0` (int) Score for player 0. This is not necessarily you.
* `score1` (int) Score for player 1.

**Response:** Whatever you want, GM don't care anymore.
