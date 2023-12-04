import os
import re


def get_file():
    __location__ = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__)))
    file = open(__location__ + os.sep + 'input.txt')
    return file.readlines()


def solution(data: list) -> tuple:
    SYMBOLS = set()
    NUMBERS = dict()
    GEARS = dict()
    L = len(data[0]) - 1

    sum_p1 = sum_p2 = 0

    for l, line in enumerate(data):
        matches = re.finditer(r'(\d+|[^.\n])', line)

        for match in matches:
            if not match.group().isdigit():
                SYMBOLS.add(l * L + match.start())
                if match.group() == '*':
                    GEARS[l * L + match.start()] = -1
            else:
                NUMBERS[(l * L + match.start(), l * L + match.end() - 1)] = int(match.group())

    for n in NUMBERS:
        for x in (n[0] - 1, n[1] + 1, *list(range(n[0] - 1 - L, n[1] + 2- L)), *list(range(n[0] - 1 + L, n[1] + 2 + L))):
            if x in SYMBOLS: 
                sum_p1 += NUMBERS[n]
            if x in GEARS:
                if GEARS[x] < 0: GEARS[x] = NUMBERS[n]
                else: sum_p2 += GEARS[x] *  NUMBERS[n]

    return sum_p1, sum_p2


if __name__ == '__main__':
    input = get_file()
    
    print(*solution(input))
