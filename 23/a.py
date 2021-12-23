HALLWAY_POINTS = (True, True, False, True, False, True, False, True, False, True, True)

COSTS = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}

HASHES = {None: 0, 'A': 1, 'B': 2, 'C': 3, 'D': 4}

def optmin(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return min(a, b)

def optadd(a, b):
    if a is None:
        return None
    if b is None:
        return None
    return a + b

class Room:
    def __init__(self, pos, slots):
        self.pos = pos
        self.slots = slots

def check_slots_availability(slots, start, end):
    a, b = sorted((start, end))
    for slot in slots[a:b+1]:
        if slot is not None:
            return False
    return True

def check_slots_availability_exclusive(slots, start, end):
    a, b = sorted((start, end))
    if a == start:
        a += 1
    else:
        b -= 1
    for slot in slots[a:b+1]:
        if slot is not None:
            return False
    return True

CACHE = {}

def cached_energy(burrow):
    index = burrow.state_index()
    energy = CACHE.get(index)
    if energy is None:
        energy = burrow.energy()
        CACHE[index] = energy
    return energy

class Burrow:
    def __init__(self, hallway, rooms):
        self.hallway = hallway
        self.rooms = rooms

    def is_solved(self):
        for goal in self.rooms:
            room = self.rooms[goal]
            if len(room.slots) != 2:
                return False
            if room.slots[0] != goal or room.slots[1] != goal:
                return False
        return True

    def upsteps(self):
        result = None
        for goal in self.rooms:
            room = self.rooms[goal]
            if any(slot != goal for slot in room.slots):
                amphipod = room.slots[-1]
                new_room = Room(room.pos, room.slots[:-1])
                base_cost = 3 - len(room.slots)
                for pos, point in enumerate(HALLWAY_POINTS):
                    if not point:
                        continue
                    cost = base_cost + abs(room.pos - pos)
                    if not check_slots_availability(self.hallway, pos, room.pos):
                        continue
                    new_hallway = self.hallway.copy()
                    new_hallway[pos] = amphipod
                    new_rooms = self.rooms.copy()
                    new_rooms[goal] = new_room
                    result = optmin(result, optadd(cost * COSTS[amphipod], cached_energy(Burrow(new_hallway, new_rooms))))
        return result

    def downsteps(self):
        result = None
        for pos, point in enumerate(HALLWAY_POINTS):
            if not point:
                continue
            amphipod = self.hallway[pos]
            if amphipod is None:
                continue
            room = self.rooms[amphipod]
            if any(i != amphipod for i in room.slots):
                continue
            new_room = Room(room.pos, room.slots + [amphipod])
            base_cost = 2 - len(room.slots)
            cost = base_cost + abs(room.pos - pos)
            if not check_slots_availability_exclusive(self.hallway, pos, room.pos):
                continue
            new_hallway = self.hallway.copy()
            new_hallway[pos] = None
            new_rooms = self.rooms.copy()
            new_rooms[amphipod] = new_room
            result = optmin(result, optadd(cost * COSTS[amphipod], cached_energy(Burrow(new_hallway, new_rooms))))
        return result

    def energy(self):
        # check for completion
        if self.is_solved():
            return 0
        # check the minimum energy cost
        return optmin(self.upsteps(), self.downsteps())

    def state_index(self):
        bits = [self.hallway[i] for i, p in enumerate(HALLWAY_POINTS) if p]
        for goal in self.rooms:
            room = self.rooms[goal]
            bits += room.slots + [None] * (2 - len(room.slots))
        result = 0
        for bit in bits:
            result *= 5
            result += HASHES[bit]
        return result

with open('input') as file:
    lines = file.readlines()
    rooms = {
        'A': Room(2, [lines[3][3], lines[2][3]]),
        'B': Room(4, [lines[3][5], lines[2][5]]),
        'C': Room(6, [lines[3][7], lines[2][7]]),
        'D': Room(8, [lines[3][9], lines[2][9]]),
    }

burrow = Burrow([None] * len(HALLWAY_POINTS), rooms)
print(cached_energy(burrow))
