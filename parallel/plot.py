from matplotlib import pyplot as plt

f = open("bins/reflect.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("Refletividade aleatória")
plt.savefig("pics/reflect.png")
plt.show()
f.close()

plt.clf()
f = open("bins/pulse.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("Pulso de Ricker")
plt.savefig("pics/pulse.png")
plt.show()
f.close()

plt.clf()
f = open("bins/signal.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("traço sísmico")
plt.savefig("pics/signal.png")
plt.show()
f.close()

plt.clf()
f = open("bins/deconv.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("traço sísmico deconvoluído")
plt.savefig("pics/deconv.png")
plt.show()
f.close()

plt.clf()
f = open("bins/iFilter.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("Filtro inverso")
plt.savefig("pics/iFilter.png")
plt.show()
f.close()

plt.clf()
f = open("bins/scf.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(-int(data[0]) + 1, -int(data[0]) + len(data[1:]) + 1)]
plt.plot(x, y)
plt.title("pulso*filtro inverso")
plt.savefig("pics/scf.png")
plt.show()
f.close()