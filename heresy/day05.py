import sys
import re

inp = sys.stdin.read()


def parse_mapping(mapping: str) -> [(int, int, int)]:
    return [tuple([int(x) for x in i.split()]) for i in mapping.split('\n')]


mappings_strs = re.findall(
    r"\w+\-to\-\w+\s+map\:\n((?:\d+\s*)+\n?)", inp)
mappingss = [parse_mapping(mappings_str.strip())
             for mappings_str in mappings_strs]


def break_range(r: (int, int), m: (int, int, int)):
    rb, rl = r
    _, mb, ml = m

    parts = [
        (min(rb, mb), min(rb + rl, mb) - min(rb, mb)),
        (max(rb, mb), min(rb + rl, mb + ml) - max(rb, mb)),
        (max(rb, mb + ml), max(rb + rl, mb + ml) - max(rb, mb + ml))
    ]

    return [(b, l) for b, l in parts if l > 0]


def translate_range(rang: (int, int), mappings: [(int, int, int)]):
    translate_with = [mapping for mapping in mappings
                      if mapping[1] <= rang[0]
                      and mapping[1] + mapping[2] >= rang[0] + rang[1]]
    print(rang, translate_with)
    if len(translate_with):
        tw = translate_with[0]
        return (rang[0] - tw[1] + tw[0], rang[1])
    else:
        return rang


seeds_strs = re.findall(r"seeds:\s+((?:\d+\s+)*)", inp)[0].strip()
seeds_nums = [int(x) for x in seeds_strs.split()]
seeds = list(zip(seeds_nums[::2], seeds_nums[1::2]))


# curr = seeds.copy()
# for mappings in mappingss:
#     new = set()
#     for c in curr:
#         for mapping in mappings:
#             for br in break_range(c, mapping):
#                 new.add(br)
#     curr = list(new)
#     print("bef", curr)
#     print(mappings)
#     curr = [translate_range(c, mappings) for c in curr]
#     print("aft", curr)
#
# print(min(curr))
