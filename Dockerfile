FROM arm64v8/haskell:9.10.1-bullseye as base

# 作業ディレクトリの作成
WORKDIR /app

# === Dependencies build stage ===
FROM base AS deps

# Stack の依存関係をインストール
COPY stack.yaml package.yaml ./
RUN stack setup && stack build --only-dependencies

# === Application build stage ===
FROM base AS build

# dependencies のビルド生成物を持ってくる
COPY --from=deps /root/.stack /root/.stack
COPY --from=deps /app/.stack-work /app/.stack-work

# アプリをビルド
COPY . .
RUN stack build

# 実行ファイルをコピー
RUN cp $(stack path --local-install-root)/bin/create-logic-hs-exe /usr/local/bin/

# コンテナ起動時に API サーバーを実行
CMD ["create-logic-hs-exe"]
