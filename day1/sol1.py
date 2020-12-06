with open("input", "r") as f:
  li = [int(li) for li in f]
  for l1 in li:
    for l2 in li:
      if l1 + l2 == 2020:
        print(l1 * l2)
