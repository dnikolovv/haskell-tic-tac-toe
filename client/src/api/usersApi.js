import * as apiClient from "./apiClient";
import { joinUrlWithRoute } from "../utils/urlUtils";

const baseUrl = joinUrlWithRoute(apiClient.BASE_URL, "/users");

export function inviteUser(userId) {
  const url = joinUrlWithRoute(baseUrl, "invite/" + userId);
  return apiClient.post(url);
}

export function getAvailableUsers() {
  const url = joinUrlWithRoute(baseUrl, "available");
  return apiClient.get(url);
}

export function createUser(username) {
  const url = joinUrlWithRoute(baseUrl, "/create/" + username);
  return apiClient.post(url);
}

export function getCurrentUser() {
  const url = joinUrlWithRoute(baseUrl, "current");
  return apiClient.get(url);
}
