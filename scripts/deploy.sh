#!/bin/sh
#
# Build Docker container and push it to Google Container Registry
#
# Environment needs $GCLOUD_PROJECT set to GCE's project.
#
#   ./scripts/deploy.sh [<docker tag>]
#
# If tag is not given, "latest" is used.
#

if [ ! -f "scripts/deploy.sh" ] && [ ! -f "Dockerfile" ]; then
    echo "ERROR: Run from project root: ./scripts/deploy.sh"
    exit 1
fi


# Exit on any error
set -e

if [ "${1}" ]; then
    TAG="${1}"
else
    TAG="latest"
fi

# No unbound variables allowed after this point
set -u

DIRNAME=$(pwd -P)
NAME=$(basename "${DIRNAME}")
REMOTE_TAG="eu.gcr.io/${GCLOUD_PROJECT}/${NAME}:${TAG}"

docker build -t "${REMOTE_TAG}" .
gcloud docker -- push "${REMOTE_TAG}"
