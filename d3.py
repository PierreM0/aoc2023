with open('input_test.txt') as f:
    str = f.read();

    lines = str.split('\n')
    aux = []
    for line in lines:
       aux.append(line.split()) 
