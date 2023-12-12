/* eslint-disable no-magic-numbers */
import { readFileSync } from "fs";
import { EOL } from "os";

type Vector = { x: number; y: number };
const asKey = (vec: Vector): string => `${vec.y}x${vec.x}`;
const asVec = (raw: string): Vector => ({
  y: parseInt(raw.split("x")[0]),
  x: parseInt(raw.split("x")[1]),
});

type Universe = Set<string>;

const parseUniverse = (buffer: string): Universe =>
  buffer
    .split(EOL)
    .slice(0, -1)
    .reduce(
      (universe, line, y) =>
        [...line].reduce(
          (universe, ch, x) =>
            ch != "." ? universe.add(asKey({ y, x })) : universe,
          universe
        ),
      new Set<string>()
    );

const expand = (universe: Universe, factor = 1): Universe => {
  const grow = (universe: Universe, key: keyof Vector): Universe => {
    let expanded = [...universe.values()];
    const keys = expanded.map((raw) => asVec(raw)[key]);
    for (let i = Math.max(...keys); i >= 0; i--) {
      if (keys.indexOf(i) == -1) {
        expanded = expanded.map((raw) => {
          const vec = asVec(raw);
          vec[key] += vec[key] > i ? factor : 0;
          return asKey(vec);
        });
      }
    }
    return new Set(expanded);
  };

  return grow(grow(universe, "x"), "y");
};

const distances = (universe: Universe): number => {
  let sum = 0;

  const galaxies = [...universe.values()];
  for (let i = 0; i < galaxies.length - 1; i++) {
    for (let j = i; j < galaxies.length - 1; j++) {
      sum += distance(asVec(galaxies[i]), asVec(galaxies[j + 1]));
    }
  }

  return sum;
};

const distance = (from: Vector, to: Vector): number =>
  Math.abs(from.x - to.x) + Math.abs(from.y - to.y);

const universe = parseUniverse(readFileSync(0, "utf-8"));

console.log(distances(expand(universe, 1)));
console.log(distances(expand(universe, 1_000_000 - 1)));
