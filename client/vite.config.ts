import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [elmPlugin({ debug: true })],
  envDir: "../",
  server: {
    allowedHosts: ["deposit-subcommittee-checklist-c.trycloudflare.com"],
    proxy: {
      "/api": {
        target: "http://localhost:3001",
        changeOrigin: true,
        secure: false,
        ws: true,
      },
    },
    hmr: {
      clientPort: 443,
    },
  },
});
