from pickletools import float8
from tokenize import Double
from matplotlib import pyplot as plt

""" f = open("reflect.data", "r")
data = (f.read()).split()
y = [int(data[i]) for i in range(len(data))]
x = [i for i in range(len(y))]
plt.stem(x, y)
plt.show()
f.close() """

f = open("pulse.data", "r")
data = (f.read()).split()
y = [float(data[i]) for i in range(len(data))]
x = [i for i in range(len(y))]
plt.plot(x, y)
plt.show()
f.close()