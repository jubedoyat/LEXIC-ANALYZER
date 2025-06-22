from lexer import lexer

with open("test.rcl", "r", encoding="utf-8") as file:
    data = file.read()

lexer.input(data)

print("TOKENS:")
while True:
    tok = lexer.token()
    if not tok:
        break
    print(f"{tok.type} ({tok.value}) - line {tok.lineno}, column {tok.lexpos}")
