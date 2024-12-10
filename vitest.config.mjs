import { defineConfig } from "vitest/config";
import path from "path";

const projectRootPath = path.resolve(".");

export default defineConfig({
  test: {
    include: ["**/__tests__/**/*_vitest.bs.mjs"],
    globals: true,
    reporters: "verbose",
    environment: "jsdom",
    coverage: {
      provider: "v8",
      include: ["src/**/*.{bs.mjs}"],
      exclude: ["node_modules/"],
      reportsDirectory: "./coverage",
      reporter: ["text", "json", "html"],
    },
  },
  resolve: {
    alias: [],
  },
});
