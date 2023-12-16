/* eslint-disable no-magic-numbers */

import { readFileSync } from "fs";

const input = readFileSync(0, "utf-8").trim().split(",");

const hash = (str: string): number =>
  str.split("").reduce((acc, ch) => ((acc + ch.charCodeAt(0)) * 17) % 256, 0);

type Lens = { label: string; focal: number };
type Dash = { kind: "dash"; label: string };
type Equal = { kind: "equal"; label: string; focal: number };
type Operation = Dash | Equal;

console.log(input.map(hash).reduce((acc, n) => acc + n, 0));

const boxes = input
  .map((line): Operation => {
    if (line.includes("=")) {
      const label = line.split("=")[0];
      const focal = Number(line.split("=")[1]);

      return {
        kind: "equal",
        label,
        focal,
      };
    } else {
      const label = line.substring(0, line.length - 1);
      return {
        kind: "dash",
        label,
      };
    }
  })
  .reduce((boxes, operation) => {
    const id = hash(operation.label);
    const box = boxes.get(id) ?? [];

    switch (operation.kind) {
      case "equal": {
        let replaced = false;

        for (let i = 0; i < box.length; i++) {
          if (box[i].label == operation.label) {
            box[i] = operation;
            replaced = true;
          }
        }

        if (!replaced) {
          box.push(operation);
        }

        boxes.set(id, box);
        break;
      }
      case "dash": {
        boxes.set(
          id,
          box.filter((lens) => lens.label != operation.label)
        );
        break;
      }
    }

    return boxes;
  }, new Map<number, Lens[]>());

console.log(
  [...boxes.entries()].reduce(
    (acc, [id, lenses]) =>
      lenses.reduce(
        (acc, lens, index) => acc + (id + 1) * (index + 1) * lens.focal,
        acc
      ),
    0
  )
);
