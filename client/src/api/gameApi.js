import * as apiClient from "./apiClient";
import { joinUrlWithRoute } from "../utils/urlUtils";

const baseUrl = joinUrlWithRoute(apiClient.BASE_URL, "game");

export function submitGameMove(gameId, cellNum) {
  const url = joinUrlWithRoute(baseUrl, "move");
  return apiClient.put(url, { cellNumber: cellNum, gameId });
}
