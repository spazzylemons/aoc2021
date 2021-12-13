from abc import abstractmethod

class Cave:
    name: str
    neighbors: list['Cave']

    def __init__(self, name):
        self.name = name
        self.neighbors = []

    def small(self):
        return self.name.islower()

    def concat(self, paths: list[list[str]]) -> list[list[str]]:
        return [path + [self.name] for path in paths]

    def visit(self, paths: list[list[str]]) -> list[list[str]]:
        if self.name == 'end':
            return self.concat(paths)
        result: list[list[str]] = []
        for neighbor in self.neighbors:
            if neighbor.small() and any(neighbor.name in path for path in paths):
                continue
            if neighbor.name == 'start':
                continue
            for neighbor_paths in neighbor.visit(self.concat(paths)):
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
