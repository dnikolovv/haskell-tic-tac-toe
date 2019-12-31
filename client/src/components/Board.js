import React from "react";
import Cell from "./Cell";
import { toPlayerSymbol } from "./../utils/gameUtils";

const Board = ({ cells, onCellClick, isBoardLocked }) => {
  if (cells) {
    cells = cells
      .map(cell => {
        return { ...cell, value: toPlayerSymbol(cell.value) };
      })
      .sort((firstCell, secondCell) => {
        return firstCell.number - secondCell.number;
      });

    return (
      <table>
        <tbody>
          <tr className="outer-row">
            <td></td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td></td>
          </tr>
          <tr>
            <td>
              <div className="outer-column-cell"></div>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[0].value}
                number={0}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[1].value}
                number={1}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[2].value}
                number={2}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <div className="outer-column-cell"></div>
            </td>
          </tr>
          <tr>
            <td>
              <div className="outer-column-cell"></div>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[3].value}
                number={3}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[4].value}
                number={4}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[5].value}
                number={5}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <div className="outer-column-cell"></div>
            </td>
          </tr>
          <tr>
            <td>
              <div className="outer-column-cell"></div>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[6].value}
                number={6}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[7].value}
                number={7}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <Cell
                onClick={onCellClick}
                value={cells[8].value}
                number={8}
                locked={!isBoardLocked}
              ></Cell>
            </td>
            <td>
              <div className="outer-column-cell"></div>
            </td>
          </tr>
          <tr className="outer-row">
            <td></td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td>
              <div className="outer-row-cell"></div>
            </td>
            <td></td>
          </tr>
        </tbody>
      </table>
    );
  }

  return <div>Loading board...</div>;
};

export default Board;
