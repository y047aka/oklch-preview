# https://hub.docker.com/r/denoland/deno
FROM mcr.microsoft.com/devcontainers/javascript-node:22
COPY --from=denoland/deno:bin-2.3.1 /deno /usr/local/bin/deno

# Install npm@latest
RUN npm install -g npm@latest

# Install nushell using npm
RUN npm install -g nushell
