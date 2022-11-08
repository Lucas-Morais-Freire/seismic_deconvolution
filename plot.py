from matplotlib import pyplot as plt

f = open("bins/reflect.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.stem(x, y)
plt.savefig("pics/reflect.png")
plt.show()
f.close()

plt.clf()
f = open("bins/pulse.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.savefig("pics/pulse.png")
plt.show()
f.close()

plt.clf()
f = open("bins/signal.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.savefig("pics/signal.png")
plt.show()
f.close()

plt.clf()
f = open("bins/deconv.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.savefig("pics/deconv.png")
plt.show()
f.close()

plt.clf()
f = open("bins/iFilter.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.savefig("pics/iFilter.png")
plt.show()
f.close()