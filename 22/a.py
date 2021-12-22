import numpy as np
import re

pattern = re.compile(r'^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$')

core = np.zeros((101, 101, 101), dtype=bool)

with open('input') as file:
    while True:
        line = file.readline()[:-1]
        if line == '':
            break
        match = pattern.match(line)
        x0 = int(match[2])
        x1 = int(match[3])
        if x0 < -50 or x1 > 50:
            continue
        y0 = int(match[4])
        y1 = int(match[5])
        if y0 < -50 or y1 > 50:
            continue
        z0 = int(match[6])
        z1 = int(match[7])
        if z0 < -50 or z1 > 50:
            continue
        value = match[1] == 'on'
        for z in range(z0, z1 + 1):
            for y in range(y0, y1 + 1):
                for x in range(x0, x1 + 1):
                    core[z + 50, y + 50, x + 50] = value

count = 0
for value in np.nditer(core):
    if value:
        count += 1
print(count)
