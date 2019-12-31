# Tic Tac Toe

<p align="center">
  <img src="./pics/game.gif" alt="Game" width="750">
</p>

A very basic web multiplayer real-time game implemented using [Servant](https://www.servant.dev/) and [Websockets](https://en.wikipedia.org/wiki/WebSocket). It can serve as an example on how to set up websockets with authentication in your Servant app.
    
# Running it locally

You need to have [`stack`](https://docs.haskellstack.org/en/stable/README/) installed together with [`npm`](https://www.npmjs.com/get-npm).

## Running the server

In the root folder of the repository, run `stack run`.

## Running the client

While inside the `client` folder, run `npm run start`.

# Contributing

There are a few TODOs at the bottom of this readme. Feel free to tackle some of those or submit any improvements that you think make sense.

# License

This project is licensed under the MIT License.

---
    
### TODOs
    
1. When a new user logs in, the "Available players" list should be updated in real time.
2. Users should be invalidated after a certain amount of time has passed without them being active.
3. When a person leaves the game/loses connection, the game should stop.
