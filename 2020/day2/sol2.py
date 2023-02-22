with open("input", "r") as f:
  count = 0
  for line in f:
    string = line.split(': ')[1]
    fst = line.split(': ')[0]
    letter = fst[-1]
    fst = fst[:-1]
    a = int(fst.split('-')[0])
    a -= 1
    b = int(fst.split('-')[1])
    b -= 1
    count += bool(string[a] == letter) != bool(string[b] == letter)

  print(count)
