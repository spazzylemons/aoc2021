from itertools import product

import re

# yield v if its endpoints are in order
def ifordered(v):
    if v[0] <= v[1]:
        yield v

# true if the segments do not overlap
def no_axis_overlap(a, b):
    return a[1] < b[0] or a[0] > b[1]

# yield segments to check when erasing a region from a cuboid
def split_axis(original, eraser):
    if no_axis_overlap(original, eraser):
        yield original
        return
    lhalf = eraser[0] < original[0]
    rhalf = eraser[1] > original[1]
    if lhalf and rhalf:
        yield original
        return
    elif lhalf:
        yield from ifordered((original[0], eraser[1]))
    elif rhalf:
        yield from ifordered((eraser[0], original[1]))
    else:
        yield eraser
    yield from ifordered((original[0], eraser[0] - 1))
    yield from ifordered((eraser[1] + 1, original[1]))

# A cuboid. Coordinates are inclusive.
class Cuboid:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    # size of cuboid along x-axis
    def xsize(self):
        return 1 + self.x[1] - self.x[0]

    # size of cuboid along y-axis
    def ysize(self):
        return 1 + self.y[1] - self.y[0]

    # size of cuboid along z-axis
    def zsize(self):
        return 1 + self.z[1] - self.z[0]

    # number of cubes in cuboid
    def area(self):
        return self.xsize() * self.ysize() * self.zsize()

    # true if overlapping on all three axes
    def intersects(self, other):
        if no_axis_overlap(self.x, other.x):
            return False
        if no_axis_overlap(self.y, other.y):
            return False
        if no_axis_overlap(self.z, other.z):
            return False
        return True

    # return a list of cuboids that do not intersect with the other cuboid
    def erase(self, other):
        # sub-cuboids to check
        xpieces = split_axis(self.x, other.x)
        ypieces = split_axis(self.y, other.y)
        zpieces = split_axis(self.z, other.z)
        result = []
        for x, y, z in product(xpieces, ypieces, zpieces):
            cuboid = Cuboid(x, y, z)
            if not cuboid.intersects(other):
                result.append(cuboid)
        return result

# regular expression to parse input file
num = r'(-?\d+)'
num_range = rf'{num}\.\.{num}'
pattern = re.compile(rf'^(on|off) x={num_range},y={num_range},z={num_range}$')

cuboids = []

with open('input') as file:
    while True:
        # read line
        line = file.readline()[:-1]
        if line == '':
            break
        # parse line
        match = pattern.match(line)
        # create new cuboid
        x = int(match[2]), int(match[3])
        y = int(match[4]), int(match[5])
        z = int(match[6]), int(match[7])
        new_cuboid = Cuboid(x, y, z)
        # list of cuboids to add to the list
        new_cuboids = []
        # iterate while removing some elements
        i = 0
        while i < len(cuboids):
            cuboid = cuboids[i]
            if cuboid.intersects(new_cuboid):
                # intersection ocurred, so get the slices of the cuboid that
                # intersect
                new_cuboids.extend(cuboid.erase(new_cuboid))
                # swap remove, since order does not matter
                cuboids[i] = cuboids[-1]
                cuboids.pop()
                # stay at index i to process next cuboid
            else:
                # move to next cuboid, as no intersection ocurred
                i += 1
        # add new cuboid slices to the list
        cuboids.extend(new_cuboids)
        # add the new cuboid if it is supposed to be added
        if match[1] == 'on':
            cuboids.append(new_cuboid)
    # sum up their areas for the answer
    print(sum(cuboid.area() for cuboid in cuboids))
