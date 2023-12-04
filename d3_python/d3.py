def is_symbol(c):
    return not c.isdigit() and c != '.'

def check_neighbors(matrix, i, j):

    # Coordonnées des voisins potentiels
    potential_neighbors = [
        (i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
        (i, j - 1),                 (i, j + 1),
        (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
    ]

    for x, y in potential_neighbors:
        if 0 <= x < len(matrix) and 0 <= y < len(matrix[x]):
            if is_symbol(matrix[x][y]):
                return True

    return False 

def get_all_num_next_to_him(line, pos):
    i = 0
    while pos + i <= len(line) -1 and line[pos+i].isnumeric():
        i += 1
    j = 0
    while pos - j >= 0 and line[pos-j].isnumeric():
        j+=1
    res = line[pos-j+1:pos+i]
    return res

with open('input_test1.txt') as f:
    str = f.read();

    lines = str.split('\n')
    aux = []
    sum = 0
    i = 0
    j = 0
    while i < len(lines):
        while j < len(lines[i]):
            if check_neighbors(lines, i, j) and lines[i][j].isnumeric():
                num = get_all_num_next_to_him(lines[i], j)
                sum += int(num)
                j += len(num)
            else:
                j += 1  
        j = 0
        i += 1
    print(sum)
    


