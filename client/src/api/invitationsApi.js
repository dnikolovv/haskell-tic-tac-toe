import * as apiClient from "./apiClient";
import { joinUrlWithRoute } from "../utils/urlUtils";

const baseUrl = joinUrlWithRoute(apiClient.BASE_URL, "/invitation");

export function accept(invitationId) {
  const url = joinUrlWithRoute(baseUrl, "accept/" + invitationId);
  return apiClient.post(url);
}

export function decline(invitationId) {
  const url = joinUrlWithRoute(baseUrl, "decline/" + invitationId);
  return apiClient.post(url);
}
