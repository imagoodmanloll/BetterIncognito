import base64

with open("exploit/env.luau", "r", encoding="utf-8") as f:
    encoded = base64.b64encode(f.read().encode("utf-8")).decode("utf-8")

with open("exploit/init.py", "r", encoding="utf-8") as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    if line.strip().startswith('INIT_SCRIPT = """'):
        lines[i] = f'    INIT_SCRIPT = """{encoded}"""\n'
        break

with open("exploit/init.py", "w", encoding="utf-8") as f:
    f.writelines(lines)
