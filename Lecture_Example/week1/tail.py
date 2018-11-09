def count(n):
    if n == 0:
        return 0
    else:
        return 1 + count(n-1)


def count_tail(n, acc):
    if n == 0:
        return acc
    else:
        return count_tail(n - 1, acc + 1)


def count_iterative(n, acc):
    while n != 0:
        n, acc = n - 1, acc + 1

    return acc
