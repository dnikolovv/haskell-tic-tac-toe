import React from "react";

const Cell = ({ number, value, onClick, locked }) => {
  const handleOnClick = () => {
    onClick(number);
  };

  return (
    <div
      className={
        "cell " +
        (value ? "taken " : " ") +
        (locked === true ? "unclickable " : "")
      }
      onClick={handleOnClick}
    >
      {value ? <div className={value}>{value}</div> : <></>}
    </div>
  );
};

export default Cell;
