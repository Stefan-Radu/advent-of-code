with open("input", "r") as f:
  count = 0
  for line in f:
    string = line.split(': ')[1]
    fst = line.split(': ')[0]
    letter = fst[-1]
    fst = fst[:-1]
    a = int(fst.split('-')[0])
    b = int(fst.split('-')[1])
    cnt = string.count(letter)
    count += a <= cnt <= b

  print(count)
