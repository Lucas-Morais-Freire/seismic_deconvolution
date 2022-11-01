FC = gfortran
FFLAGS = -O3 -Wall -Wextra -fopenmp
MAIN = main.f90
SRCS = seisDeconv.f90
PLOT = plot.py
BIN = ${MAIN:.f90=.exe}

compile: ${BIN}

${BIN}: ${MAIN} ${SRCS}
	${FC} ${FFLAGS} ${SRCS} ${MAIN} -o ${BIN}

run: ${BIN}
	@./${BIN}

plot: run
	python ${PLOT}