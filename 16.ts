/* eslint-disable complexity */
import { readFileSync } from "fs";
import { EOL } from "os";

const grid = readFileSync(0, "utf8")
  .split(EOL)
  .filter((line) => line != "")
  .map((line) => line.split(""));

type Direction = "north" | "east" | "south" | "west";
type Vec = { y: number; x: number };
type Beam = { direction: Direction; vec: Vec };

const key = (beam: Beam): string =>
  `${beam.vec.y}|${beam.vec.x}|${beam.direction}`;

const pipe = (beam: Beam, vec: Vec, visited: Set<string>): Vec[] => {
  if (beam.direction == "west" || beam.direction == "east") {
    return [
      vec,
      ...bounce(grid, { direction: "north", vec: { ...vec } }, visited),
      ...bounce(grid, { direction: "south", vec: { ...vec } }, visited),
    ];
  } else {
    return [vec, ...bounce(grid, { ...beam, vec: { ...vec } }, visited)];
  }
};

const dash = (beam: Beam, vec: Vec, visited: Set<string>): Vec[] => {
  if (beam.direction == "north" || beam.direction == "south") {
    return [
      vec,
      ...bounce(grid, { direction: "west", vec: { ...vec } }, visited),
      ...bounce(grid, { direction: "east", vec: { ...vec } }, visited),
    ];
  } else {
    return [vec, ...bounce(grid, { ...beam, vec: { ...vec } }, visited)];
  }
};

const mirror = (
  beam: Beam,
  vec: Vec,
  mapping: Record<Direction, Direction>,
  visited: Set<string>
): Vec[] => [
  vec,
  ...bounce(
    grid,
    { direction: mapping[beam.direction], vec: { ...vec } },
    visited
  ),
];

const bounce = (
  grid: string[][],
  beam: Beam,
  visited: Set<string> = new Set()
): Vec[] => {
  if (visited.has(key(beam))) {
    return [];
  }

  visited.add(key(beam));

  const vec = beam.vec;

  switch (beam.direction) {
    case "north": {
      vec.y -= 1;
      break;
    }
    case "east": {
      vec.x += 1;
      break;
    }
    case "south": {
      vec.y += 1;
      break;
    }
    case "west": {
      vec.x -= 1;
      break;
    }
  }

  const ch = (grid[vec.y] ?? [])[vec.x] ?? undefined;

  switch (ch) {
    case undefined:
      return [];
    case ".":
      return [vec, ...bounce(grid, { ...beam, vec: { ...vec } }, visited)];
    case "-":
      return dash(beam, vec, visited);
    case "|":
      return pipe(beam, vec, visited);
    case "\\":
      return mirror(
        beam,
        vec,
        { north: "west", east: "south", south: "east", west: "north" },
        visited
      );
    case "/": {
      return mirror(
        beam,
        vec,
        { north: "east", east: "north", south: "west", west: "south" },
        visited
      );
    }
  }

  return [];
};

const energized = (grid: string[][], beam: Beam): number =>
  new Set(bounce(grid, beam).map((vec) => key({ direction: "north", vec })))
    .size;

console.log(energized(grid, { direction: "east", vec: { y: 0, x: -1 } }));

let max = 0;
const xs = [...Array(grid[0].length).keys()];
const ys = [...Array(grid.length).keys()];

for (let i = 0; i < ys.length; i++) {
  max = Math.max(
    max,
    energized(grid, { direction: "east", vec: { y: ys[i], x: -1 } }),
    energized(grid, { direction: "west", vec: { y: ys[i], x: xs.at(-1)! + 1 } })
  );
}

for (let i = 0; i < xs.length; i++) {
  max = Math.max(
    max,
    energized(grid, { direction: "south", vec: { y: -1, x: xs[i] } }),
    energized(grid, {
      direction: "north",
      vec: { y: ys.at(-1)! + 1, x: xs[i] },
    })
  );
}

console.log(max);
