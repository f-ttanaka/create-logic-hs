.PHONY: bootstrap docker run

docker:
	docker build -t create-logic .

run:
	docker run -it create-logic
