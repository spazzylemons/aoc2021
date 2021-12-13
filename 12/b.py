from abc import abstractmethod

class Cave:
    name: str
    neighbors: list['Cave']

    def __init__(self, name):
        self.name = name
        self.neighbors = []

    def small(self):
        return self.name.islower()

    @staticmethod
    def valid_cave(path: list[str]) -> bool:
        smalls = set()
        for cave in path:
            if cave == 'start' or cave == 'end':
                continue
            if not cave.islower():
                continue
            smalls.add(cave)
        many_count = 0
        for cave in smalls:
            if path.count(cave) > 2:
                return False
            elif path.count(cave) == 2:
                many_count += 1
            if many_count == 2:
                return False
        return True

    def concat(self, paths: list[list[str]]) -> list[list[str]]:
        return list(filter(Cave.valid_cave, (path + [self.name] for path in paths)))

    def visit(self, paths: list[list[str]]) -> list[list[str]]:
        new_paths = self.concat(paths)
        if new_paths == []:
            return new_paths
        if self.name == 'end':
            return new_paths
        result: list[list[str]] = []
        for neighbor in self.neighbors:
            if neighbor.name == 'start':
                continue
            for neighbor_paths in neighbor.visit(new_paths):
                result.append(neighbor_paths)
        return result

caves: dict[str, Cave] = {}

with open('input') as file:
    for line in file.readlines():
        u_name, v_name = line[:-1].split('-')
        if u_name not in caves:
            caves[u_name] = Cave(u_name)
        if v_name not in caves:
            caves[v_name] = Cave(v_name)
        caves[u_name].neighbors.append(caves[v_name])
        caves[v_name].neighbors.append(caves[u_name])

print(len(caves['start'].visit([[]])))
