from matplotlib import pyplot as plt

f = open("reflect.data", "r")
data = (f.read()).split()
y = [int(data[i]) for i in range(len(data))]
x = [i for i in range(len(y))]
plt.stem(x, y)
plt.show()