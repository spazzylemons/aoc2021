import numpy as np

def bitchar(b):
    return '#' if b else '.'

class Image:
    def __init__(self, rules, pixels):
        self.horizon = False
        self.rules = rules
        self.pixels = np.array(pixels)

    def evolve(self):
        extpixels = np.pad(self.pixels, ((2, 2), (2, 2)), constant_values=self.horizon)
        newpixels = np.zeros(tuple(i + 2 for i in self.pixels.shape), dtype=bool)
        h, w = (i + 2 for i in self.pixels.shape)
        for y, x in np.ndindex((h, w)):
            rule = 0
            for dy in range(3):
                for dx in range(3):
                    rule <<= 1
                    rule |= extpixels[y + dy, x + dx]
            newpixels[y, x] = self.rules[rule]
        self.pixels = newpixels
        self.horizon = self.rules[0b111111111 if self.horizon else 0b000000000]

    def display(self):
        print('horizon = {}'.format(bitchar(self.horizon)))
        for line in self.pixels:
            print(''.join(bitchar(b) for b in line))

    def count(self):
        if self.horizon:
            raise ValueError('infinite number of pixels are lit')
        count = 0
        for p in np.nditer(self.pixels):
            if p:
                count += 1
        return count

with open('input') as file:
    rules = [i == '#' for i in file.readline()[:-1]]
    file.readline()
    pixels = []
    y = 32
    while True:
        line = file.readline()[:-1]
        if line == '':
            break
        pixels.append([c == '#' for c in line])

image = Image(rules, pixels)

for _ in range(2):
    image.evolve()

print(image.count())
