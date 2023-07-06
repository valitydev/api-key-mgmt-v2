ARG OTP_VERSION

# Build the release
FROM docker.io/library/erlang:${OTP_VERSION} AS builder
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install thrift compiler
ARG THRIFT_VERSION
ARG TARGETARCH
RUN wget -q -O- "https://github.com/valitydev/thrift/releases/download/${THRIFT_VERSION}/thrift-${THRIFT_VERSION}-linux-${TARGETARCH}.tar.gz" \
    | tar -xvz -C /usr/local/bin/

# Hack ssh fetch and copy sources
ARG FETCH_TOKEN
RUN git config --global url."https://${FETCH_TOKEN}@github.com/".insteadOf ssh://git@github.com/ ;\
    mkdir /build
COPY . /build/

# Build the release
WORKDIR /build
RUN rebar3 compile && \
    rebar3 as prod release

# Make a runner image
FROM docker.io/library/erlang:${OTP_VERSION}-slim

ARG SERVICE_NAME

# Set env
ENV CHARSET=UTF-8
ENV LANG=C.UTF-8

# Set runtime
WORKDIR /opt/${SERVICE_NAME}

COPY --from=builder /build/_build/prod/rel/${SERVICE_NAME} /opt/${SERVICE_NAME}

# Set up migration
COPY --from=builder /build/migrations /opt/${SERVICE_NAME}/migrations
COPY --from=builder /build/.env /opt/${SERVICE_NAME}/.env

ENV WORK_DIR=/opt/${SERVICE_NAME}

RUN echo "#!/bin/sh" >> /entrypoint.sh && \
    echo "exec /opt/${SERVICE_NAME}/bin/${SERVICE_NAME} foreground" >> /entrypoint.sh && \
    chmod +x /entrypoint.sh
ENTRYPOINT []
CMD ["/entrypoint.sh"]

EXPOSE 8022
