import React, { useState, useEffect } from "react";
import * as webSocketClient from "../api/webSocketClient";
import * as gameApi from "../api/gameApi";
import Board from "./Board";
import { connect } from "react-redux";
import { toPlayerSymbol } from "../utils/gameUtils";

const Game = ({ currentUser, history, match }) => {
  useEffect(() => {
    webSocketClient.connectTo("/game/" + match.params.gameId, message => {
      setGameState(JSON.parse(message.data));
    });
  }, [match.params.gameId]);

  const [gameState, setGameState] = useState({});

  const submitGameMove = cellNum => {
    gameApi.submitGameMove(gameState.id, cellNum);
  };

  const isCurrentPlayerNext = gameState.nextPlayer == currentUser.id;
  const isBoardLocked = !gameState.isOver && isCurrentPlayerNext;
  const currentPlayerNumber = gameState.player1 == currentUser.id ? 1 : 2;

  return (
    <div className="game-container">
      {gameState.isOver && gameState.winner ? (
        gameState.winner == currentUser.id ? (
          <div className="game-result">You won!</div>
        ) : (
          <div className="game-result">You lost :(</div>
        )
      ) : (
        <></>
      )}
      {gameState.isOver && !gameState.winner ? (
        <div className="game-result">Game is a draw.</div>
      ) : (
        <></>
      )}
      {!gameState.isOver ? (
        isCurrentPlayerNext ? (
          <div className="next-player">
            You are next ({toPlayerSymbol(currentPlayerNumber)}).
          </div>
        ) : (
          <div className="next-player">The other player is next.</div>
        )
      ) : (
        <></>
      )}
      {gameState.board ? (
        <Board
          cells={gameState.board.cells}
          onCellClick={submitGameMove}
          isBoardLocked={isBoardLocked}
        ></Board>
      ) : (
        <></>
      )}
      {gameState.isOver ? (
        <div className="go-back-button-container">
          <button
            className="go-back-button"
            onClick={() => history.push("/home")}
          >
            Go back
          </button>
        </div>
      ) : (
        <></>
      )}
    </div>
  );
};

function mapStateToProps(state) {
  return {
    currentUser: state.currentUser
  };
}

export default connect(mapStateToProps, null)(Game);
