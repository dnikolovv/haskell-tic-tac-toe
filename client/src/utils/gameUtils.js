export function toPlayerSymbol(playerNumber) {
  switch (playerNumber) {
    case 1:
      return "X";
    case 2:
      return "O";
    default:
      return null;
  }
}
