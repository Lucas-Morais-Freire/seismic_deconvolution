from matplotlib import pyplot as plt

f = open("/home/lucasmf/Documents/seismic_deconvolution/reflect.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(int(data[0]), int(data[0]) + len(data[1:]))]
plt.stem(x, y)
plt.show()
f.close()

f = open("/home/lucasmf/Documents/seismic_deconvolution/reflect.data", "r")
data = (f.read()).split()
y = [float(data[1:][i]) for i in range(len(data[1:]))]
x = [i for i in range(int(data[0]), int(data[0]) + len(data[1:]))]
plt.plot(x, y)
plt.show()
f.close()

print(data[2])