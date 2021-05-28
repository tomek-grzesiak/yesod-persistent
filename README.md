# yesod-poc

# db setup
podman run  -p 5432:5432 -e postgres_DB=postgres -e POSTGRES_PASSWORD=postgres -d docker.io/library/postgres:10.6