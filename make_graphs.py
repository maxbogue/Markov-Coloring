#! /usr/bin/env python

from sys import argv
from random import sample
from collections import Counter

def print_graph(n, m):
    counter = Counter()
    edges = set([])
    for _ in range(m):
        edge = tuple(sorted(sample(range(n), 2)))
        while edge in edges:
            edge = tuple(sorted(sample(range(n), 2)))
        edges.add(edge)
        counter.update(list(edge))
    q = counter.most_common()[0][1] + 3
    print "%s %s %s" % (n, m, q)
    for edge in sorted(edges):
        print "%s %s" % edge

if __name__ == "__main__":
    n = int(argv[1])
    m = int(argv[2])
    print_graph(n, m)
