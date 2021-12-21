from functools import cache

# calculate all possible rolls of three quantum dice
rolls = []
for one_d3 in (1, 2, 3):
    for two_d3 in (1, 2, 3):
        for three_d3 in (1, 2, 3):
            rolls.append(one_d3 + two_d3 + three_d3)

@cache
def game(
    p1_pos: int, p1_score: int,
    p2_pos: int, p2_score: int):
    w1, w2 = 0, 0
    for roll in rolls:
        new_pos = (p1_pos + roll) % 10
        new_score = p1_score + new_pos + 1
        if new_score >= 21:
            w1 += 1
        else:
            g2, g1 = game(
                p2_pos, p2_score,
                new_pos, new_score,
            )
            w1 += g1
            w2 += g2
    return w1, w2

with open('input') as file:
    p1 = int(file.readline()[:-1].split(' ')[-1]) - 1
    p2 = int(file.readline()[:-1].split(' ')[-1]) - 1

p1_count, p2_count = game(p1, 0, p2, 0)
print(max(p1_count, p2_count))
