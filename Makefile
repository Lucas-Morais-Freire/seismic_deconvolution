FC = gfortran
PC = python
FFLAGS = -O3 -Wall -Wextra -fopenmp
MAIN = teste.f90
SRCS = seisDeconv.f90
SRCDIR = srcs
PLOT = plot.py
BIN = ${MAIN:.f90=.exe}

compile: ${BIN}

${BIN}: ${MAIN} ${SRCDIR}/${SRCS}
	${FC} ${FFLAGS} ${SRCDIR}/${SRCS} ${MAIN} -o ${BIN}

run: ${BIN}
	@./${BIN}

plotAll:
	@${PC} ${PLOT}

random: run
	@${PC} ${PLOT}
