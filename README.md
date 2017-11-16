# Pja3 compiler optimisation

## Project Structure

The main program for this package is jlite_main.ml.

It makes use of the data structure file "jlite_structs.ml" to construct parse trees, and "ir3_structs.ml" to construct "ir3" intermediate three-address code.

Lastly, it provides the data sturture file "arm_structs.ml" to construct the assembly code.

## Commands

To compile the project, run

```bash
make
```

To run unit tests, run

```bash
make unit
```

To remove compiled files, run

```bash
make clean
```

To run the compiled code, download the docker image and run it by

```bash
docker pull falconets/pja3
# Assumes pwd = /pja3 folder
docker run -d -it --name proj --mount type=bind,source="$(pwd)",target=/usr/local/src/pja3 falconets/pja3:latest
```

To access the docker container to run the arm code

```bash
docker exec -it proj /bin/bash
# after accessing bash
make run
```

## Notes

There are some test cases available in the directory "armTests". In this directory, we provide the test cases (files end with extension "j"), and sample compiled assembly codes (files end with extension "s"). However,
please note that the assembly code you produce will definitely be different from these codes.
