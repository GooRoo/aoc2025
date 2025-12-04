#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.14"
# dependencies = []
# ///

import sys


def read_grid(path: str) -> list[list[int]]:
    with open(path) as f:
        lines = [line.rstrip('\n') for line in f if line.strip()]
    width = len(lines[0]) + 2
    grid = [[0] * width]  # top border
    for line in lines:
        row = [0] + [1 if ch == '@' else 0 for ch in line] + [0]
        grid.append(row)
    grid.append([0] * width)  # bottom border
    return grid, width, len(grid)


def take_paper_rolls(grid: list[list[int]]) -> int:
    height, width = len(grid), len(grid[0])
    to_remove = []
    for y in range(1, height - 1):
        for x in range(1, width - 1):
            if grid[y][x] == 0:
                continue
            elif sum(
                grid[y + dy][x + dx]
                for dy in (-1, 0, 1)
                for dx in (-1, 0, 1)
            ) < 5:
                to_remove.append((y, x))
    for y, x in to_remove:
        grid[y][x] = 0
    return len(to_remove)


def solve(grid: list[list[int]]) -> int:
    total = 0
    while (taken := take_paper_rolls(grid)) > 0:
        total += taken
    return total


def main() -> None:
    filename = sys.argv[1] if len(sys.argv) > 1 else "day04/data/task.example"
    grid, width, height = read_grid(filename)
    print(f"Grid size: {height}×{width}")
    result = solve(grid)
    print(f"Result: {result}")


if __name__ == "__main__":
    main()
