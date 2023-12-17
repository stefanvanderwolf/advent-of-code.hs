/* eslint-disable no-magic-numbers */
/* eslint-disable complexity */
import { readFileSync } from "fs";
import { EOL } from "os";

type Line = { record: string[]; groups: number[] };

const lines: Line[] = readFileSync(0, "utf8")
  .split(EOL)
  .filter((line) => line.length != 0)
  .map((line) => ({
    record: line.split(" ")[0].split(""),
    groups: line.split(" ")[1].split(",").map(Number),
  }));

const unfold = ({ record, groups }: Line): Line => ({
  record: Array<string>(5).fill(record.join("")).join("?").split(""),
  groups: Array<number[]>(5).fill(groups).flat(),
});

const memoize = (fn: (line: Line) => number): ((line: Line) => number) => {
  const cache = new Map<string, number>();

  return (line: Line) => {
    const key = `${line.record.join(",")}|||${line.groups.join(",")}`;
    if (!cache.has(key)) {
      cache.set(key, fn(line));
    }
    return cache.get(key)!;
  };
};

const arrangements = memoize(({ record, groups }: Line): number => {
  if (groups.length == 0) {
    return record.includes("#") ? 0 : 1;
  }

  if (record.length == 0) {
    return 0;
  }

  const group = groups[0];

  switch (record[0]) {
    case ".":
      return arrangements({ record: record.slice(1), groups });
    case "#":
      for (let i = 0; i < group; i++) {
        if (record[i] != "#" && record[i] != "?") {
          return 0;
        }
      }

      if (
        record[group] == "?" ||
        record[group] == "." ||
        record[group] == null
      ) {
        return arrangements({
          record: record.slice(group + 1),
          groups: groups.slice(1),
        });
      }

      return 0;
    case "?":
      return (
        arrangements({
          record: [".", ...record.slice(1)],
          groups,
        }) +
        arrangements({
          record: ["#", ...record.slice(1)],
          groups,
        })
      );
    default:
      return 0;
  }
});

console.log(lines.map(arrangements).reduce((acc, n) => acc + n, 0));
console.log(
  lines
    .map(unfold)
    .map(arrangements)
    .reduce((acc, n) => acc + n, 0)
);
