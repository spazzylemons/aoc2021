class Player:
    def __init__(self, pos):
        self.pos = pos
        self.score = 0

    def move(self, amount):
        self.pos = (self.pos + amount) % 10
        self.score += self.pos + 1
        return self.score >= 1000

class DiracDice:
    def __init__(self, p1, p2):
        self.die = 0
        self.dice_rolls = 0
        self.p1 = Player(p1)
        self.p2 = Player(p2)

    def roll3(self):
        sum = 0
        for _ in range(3):
            sum += self.die + 1
            self.die = (self.die + 1) % 100
        self.dice_rolls += 3
        return sum

    def turn(self):
        if self.p1.move(self.roll3()):
            return self.p2.score * self.dice_rolls
        if self.p2.move(self.roll3()):
            return self.p1.score * self.dice_rolls

with open('input') as file:
    p1 = int(file.readline()[:-1].split(' ')[-1]) - 1
    p2 = int(file.readline()[:-1].split(' ')[-1]) - 1

game = DiracDice(p1, p2)

while True:
    result = game.turn()
    if result is not None:
        print(result)
        break
