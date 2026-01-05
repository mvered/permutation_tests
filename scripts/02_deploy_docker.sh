#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# --- 1. CONFIGURATION ---
AWS_ACCOUNT_ID="756916139833"
REGION="us-east-2"
REPO_NAME="permutation-sim"
IMAGE_TAG="latest"

# Construct the full ECR URL
ECR_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/${REPO_NAME}"

echo "Starting deployment for $REPO_NAME..."

# --- 2. DOCKER BUILD ---
# This builds the image using the Dockerfile in /docker/ but sees the whole project
echo "Building Docker image..."
docker build -t $REPO_NAME -f docker/Dockerfile .

# --- 3. ECR AUTHENTICATION ---
# This logs WSL Docker into the AWS Cloud
echo "Authenticating with AWS ECR..."
aws ecr get-login-password --region $REGION | \
    docker login --username AWS --password-stdin $ECR_URL

# --- 4. TAG AND PUSH ---
# This uploads the local Dockerfile image to the AWS cloud registry
echo "Pushing image to ECR: ${ECR_URL}:${IMAGE_TAG}"
docker tag "${REPO_NAME}:latest" "${ECR_URL}:${IMAGE_TAG}"
docker push "${ECR_URL}:${IMAGE_TAG}"

echo "Deployment successful! The image is now in the cloud."