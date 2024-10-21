FC = gfortran
PC = python3
FFLAGS = -Wall -Wextra -g # -O3
SRCS = srcs/seisDeconv.f90 main.f90 
PLOT = plot.py
TARGET = main.exe
INCLUDE_PATHS = srcs

OBJS = $(patsubst %.f90,%.o,$(SRCS))
INCLUDE_FLAGS = $(patsubst %,-I%,$(INCLUDE_PATHS))

# test:
# 	@echo $(MODS)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $(TARGET) $(INCLUDE_FLAGS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $^ -o $@ $(INCLUDE_FLAGS)

clean:
	rm -f $(TARGET) $(OBJS)