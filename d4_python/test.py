def get_list_of_num_in_prelist(line):
   posmin = line.find(':') + 1
   posmax = line.find('|')
   prelist = list(filter(lambda s: s != '', line[posmin:posmax].split(' ')))
   return prelist

def get_list_of_num_in_postlist(line):
   posmin = line.find('|') + 1
   postlist = list(filter(lambda s: s != '', line[posmin:].split(' ')))
   return postlist



with open("input.txt") as f:
   lines = f.read().split('\n')
   sum = 0
   how_many_cards = {0: 0, 1: 1}
   for i, line in enumerate(lines):
      num_card = i + 1

      if (len(line) == 0):
         continue

      list_of_num =  get_list_of_num_in_prelist(line)
      list_of_num_in_postlist = get_list_of_num_in_postlist(line)

      nb = 0 
      for num in list_of_num_in_postlist:
         if num in list_of_num:
            nb += 1

      if (num_card not in how_many_cards):
         how_many_cards[num_card] = 1

      for i in range(1, nb + 1):
         if (num_card+i) in how_many_cards:
            how_many_cards[num_card + i] += how_many_cards[num_card]
         else:
            how_many_cards[num_card + i] = how_many_cards[num_card] + 1


      print(num_card, how_many_cards[num_card])

   for i in range(len(how_many_cards)):
      sum += how_many_cards[i]

   print(sum)
