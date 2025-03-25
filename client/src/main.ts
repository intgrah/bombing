// import {
//   CommandResponse,
//   DiscordSDK,
//   patchUrlMappings,
// } from "@discord/embedded-app-sdk";

// patchUrlMappings([], { patchWebSocket: true });

// ---- Elm stuff ----

import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("app")!,
});

window.dispatchEvent(new Event("resize"));

// ---- Discord stuff ----

// const discordSdk = new DiscordSDK(import.meta.env.VITE_DISCORD_CLIENT_ID);
// await discordSdk.ready();

// const { code } = await discordSdk.commands.authorize({
//   client_id: import.meta.env.VITE_DISCORD_CLIENT_ID,
//   response_type: "code",
//   state: "",
//   prompt: "none",
//   scope: ["identify", "guilds", "applications.commands"],
// });

// const response = await fetch("/.proxy/api/token", {
//   method: "POST",
//   headers: {
//     "Content-Type": "application/json",
//   },
//   body: JSON.stringify({
//     code,
//   }),
// });

// const { access_token }: { access_token: string } = await response.json();

// const auth: CommandResponse<"authenticate"> =
//   await discordSdk.commands.authenticate({
//     access_token: access_token.substring(1, access_token.length - 1),
//   });

// ---- Send the authentication result to Elm! ----

// const socket = new WebSocket(`/.proxy/api/socket?token=${auth.access_token}`);
const socket = new WebSocket(`/api/socket`);

console.log(socket.readyState);

app.ports.internalSend.subscribe((data: string) => socket.send(data));

// socket.addEventListener("message", (event) => {
//   console.log("DATADATADATADATADATADATA", event.data);
//   app.ports.internalReceive.send(event.data);
// });

// app.ports.authenticated.send(auth);

// ====================================================================

// ---- RPCs ----

// async function getVoiceChannel(
//   channelId: string
// ): Promise<CommandResponse<"getChannel">> {
//   return await discordSdk.commands.getChannel({ channel_id: channelId });
// }
