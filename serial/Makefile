FC = gfortran
FFLAGS = -Wall -Wextra -g # -O3
TARGET = main.exe
SRCS = mods/seisDeconv.f90 main.f90
INCLUDE_PATHS = mods

OBJS = $(patsubst %.f90,%.o,$(SRCS))
INCLUDE_FLAGS = $(patsubst %,-I%,$(INCLUDE_PATHS))

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $(TARGET) $(INCLUDE_FLAGS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@ -J$(@D) $(INCLUDE_FLAGS)

clean:
	rm -f $(OBJS) $(TARGET) *.mod $(patsubst %,%/*.mod,$(INCLUDE_PATHS))