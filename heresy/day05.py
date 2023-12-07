import sys
import re

# PARSING INPUT

inp = sys.stdin.read()


def parse_mapping(mapping: str) -> list[tuple[int, int, int]]:
    return [
        (vals[0], vals[1], vals[2])
        for vals in [[int(x) for x in i.split()] for i in mapping.split("\n")]
    ]


seeds_strs = re.findall(r"seeds:\s+((?:\d+\s+)*)", inp)[0].strip()
seeds_nums = [int(x) for x in seeds_strs.split()]
seeds: list[tuple[int, int]] = list(zip(seeds_nums[::2], seeds_nums[1::2]))

mappings_strs = re.findall(r"\w+\-to\-\w+\s+map\:\n((?:\d+\s*)+\n?)", inp)
mappingss: list[list[tuple[int, int, int]]] = [
    parse_mapping(mappings_str.strip()) for mappings_str in mappings_strs
]


def break_range(a: tuple[int, int], b: tuple[int, int]) -> list[tuple[int, int]]:
    astart, alen = a
    aend = astart + alen
    bstart, blen = b
    bend = bstart + blen

    parts = [
        (min(astart, bstart), min(aend, bstart)),
        (max(bstart, astart), min(aend, bend)),
        (max(bend, astart), max(aend, bend)),
    ]
    parts = [(start, end - start) for start, end in parts]

    return [(start, length) for start, length in parts if length > 0]


def translate_range(
    r: tuple[int, int], mappings: list[tuple[int, int, int]]
) -> tuple[int, int]:
    rstart, rlen = r
    res = None
    for mapping in mappings:
        _, mstart, mlen = mapping
        if mstart <= rstart <= (mstart + mlen):
            res = mapping
            break

    if res is None:
        return r

    mto, mstart, mlen = res
    return (rstart - mstart + mto, rlen)


source = seeds
for mappings in mappingss:
    for _, start, length in mappings:
        new_source: list[tuple[int, int]] = list()
        for seed in source:
            new_source.extend(break_range(seed, (start, length)))
        source = new_source

    translated: list[tuple[int, int]] = []
    for seed in source:
        translated.append(translate_range(seed, mappings))

    source = translated

print(min(source)[0])
