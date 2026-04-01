def has_period(s: str) -> bool:
    n = len(s)

    for d in range(1, n):          # Seq.init (n - 1) ((+) 1)
        if n % d != 0:
            continue

        first = s[:d]

        ok = True
        for i in range(d, n, d):
            if s[i:i + d] != first:
                ok = False
                break

        if ok:
            return True

    return False


def solve():
    with open("puzzle2.in", "r") as f:
        content = f.read().strip()

    ranges = []
    for part in content.split(","):
        a, b = part.split("-")
        ranges.append((int(a), int(b)))

    ans = 0

    for l, r in ranges:
        for x in range(l, r + 1):   # Seq.init (r - l + 1) ((+) l)
            if has_period(str(x)):
                ans += x

    print(ans)


if __name__ == "__main__":
    solve()
