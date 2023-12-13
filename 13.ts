/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-magic-numbers */
import { readFileSync } from "fs";
import { EOL } from "os";

const asInt = (line: string[]): number =>
  line
    .map((ch, i): [string, number] => [ch, i])
    .filter(([ch]) => ch == "#")
    .reduce((bits, [_, i]) => (bits |= 1 << (line.length - 1 - i)), 0);

const mirrors = readFileSync(0, "utf-8")
  .split(EOL + EOL)
  .map((mirror) =>
    mirror
      .split(EOL)
      .filter((l) => l.length != 0)
      .map((line) => line.split(""))
  )
  .map((mirror) => ({
    horizontal: mirror.map(asInt),
    vertical: mirror[0]
      .map((_, col) => mirror.map((row) => row[col]))
      .map(asInt),
  }));

const find = (mirror: number[], factor: number, delta = 0): number | null => {
  const illegal = delta != 0 ? find(mirror, 1, 0) ?? -1 : -1;

  for (let i = 0; i < mirror.length; i++) {
    if (illegal == i) continue;

    let dx = diff(mirror[i], mirror[i + 1]);
    if (dx <= delta) {
      const n = Math.min(i, mirror.length - 2 - i);
      const before = mirror.slice(i - n, i);
      const after = mirror.slice(i + 2, i + 2 + n).reverse();

      for (let i = 0; i < before.length; i++) {
        dx += diff(before[i], after[i]);
      }

      if (dx == delta) {
        return (i + 1) * factor;
      }
    }
  }
  return null;
};

const diff = (a: number, b: number): number =>
  [...Array(32).keys()].reduce(
    (dx, i) => dx + ((((a >> i) & 1) + ((b >> i) & 1)) % 2),
    0
  );

console.log(
  [0, 1].map((delta) =>
    mirrors
      .map(
        (mirror) =>
          find(mirror.vertical, 1, delta) ??
          find(mirror.horizontal, 100, delta) ??
          0
      )
      .reduce((acc, n) => acc + n, 0)
  )
);
