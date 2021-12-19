BRANCH = 'B'

def explode(l: list, index: int, depth: int) -> tuple[bool, int]:
    if l[index] == BRANCH:
        if depth == 4:
            previndex = index
            while previndex >= 0 and l[previndex] == BRANCH:
                previndex -= 1
            if previndex != -1:
                l[previndex] += l[index + 1]
            nextindex = index + 3
            while nextindex < len(l) and l[nextindex] == BRANCH:
                nextindex += 1
            if nextindex != len(l):
                l[nextindex] += l[index + 2]
            l.pop(index)
            l.pop(index)
            l[index] = 0
            return True, None
        index += 1
        i, index = explode(l, index, depth + 1)
        if i:
            return True, None
        return explode(l, index, depth + 1)
    else:
        return False, index + 1

def split(l: list, index: int) -> tuple[bool, int]:
    if l[index] == BRANCH:
        index += 1
        i, index = split(l, index)
        if i:
            return True, None
        return split(l, index)
    elif l[index] >= 10:
        value = l[index]
        l.insert(index, None)
        l.insert(index, None)
        l[index] = BRANCH
        l[index + 1] = value // 2
        l[index + 2] = (value + 1) // 2
        return True, None
    else:
        return False, index + 1

def reduce(l: list):
    while True:
        i, _ = explode(l, 0, 0)
        if i:
            continue
        i, _ = split(l, 0)
        if i:
            continue
        break

def magnitude(l: list, index: int) -> tuple[int, int]:
    if l[index] == BRANCH:
        index += 1
        i, index = magnitude(l, index)
        j, index = magnitude(l, index)
        return 3 * i + 2 * j, index
    else:
        return l[index], index + 1

def assemble_tree(l):
    if isinstance(l, list):
        return [BRANCH] + assemble_tree(l[0]) + assemble_tree(l[1])
    else:
        return [l]

with open('input') as file:
    tree = assemble_tree(eval(file.readline()))
    while line := file.readline():
        tree = [BRANCH] + tree + assemble_tree(eval(line))
        reduce(tree)
print(magnitude(tree, 0)[0])
