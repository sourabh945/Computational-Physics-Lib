import math
with open("circle.dat","w") as file:
    data = []
    for i in range(360):
        x = 100*math.cos(i)
        y = 100*math.sin(i)
        data.append((f"{x}\t{y}\n"))
    file.writelines(data)
    file.close()