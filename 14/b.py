HALF = 20

def get_pair_insertion(string: str):
    return string[0:2], string[6]

def expand_concat(a: str, b: str, rules: dict):
    return a + rules[a + b]

def run_pair(p: str, rules: dict):
    for _ in range(HALF):
        p = ''.join(expand_concat(p[i], p[i+1], rules) for i in range(len(p) - 1)) + p[-1]
    return p

def tally_counts(p: str, counts: dict):
    for element in p:
        if element not in counts:
            counts[element] = 0
        counts[element] += 1

def add_counts(counts_from: str, counts: dict):
    for c in counts_from:
        if c not in counts:
            counts[c] = 0
        counts[c] += counts_from[c]

with open("input") as file:
    rules = {}
    polymer = file.readline()[:-1]
    file.readline()
    while True:
        line = file.readline()[:-1]
        if not line:
            break
        pair, insertion = get_pair_insertion(line)
        rules[pair] = insertion
ran_pairs = {}
counts = {}
for pair in rules:
    ran_pairs[pair] = run_pair(pair, rules)
ran_counts = {}
for pair in ran_pairs:
    ran_count = {}
    tally_counts(ran_pairs[pair][:-1], ran_count)
    ran_counts[pair] = ran_count
i = 0
while i + 1 < len(polymer):
    p1 = ran_pairs[polymer[i] + polymer[i + 1]]
    j = 0
    while j + 1 < len(p1):
        add_counts(ran_counts[p1[j] + p1[j + 1]], counts)
        j += 1
    i += 1
tally_counts(polymer[-1], counts)
print(counts[max(counts, key=counts.get)] - counts[min(counts, key=counts.get)])
