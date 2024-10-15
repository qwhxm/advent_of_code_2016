"""adventofcode.com/2016/day/12"""
# NOTE doesn't support jnz instructions where jump offset is stored in
#      a register (e.g. jnz a c) - not mentioned or used in this puzzle
import re

code = [
    "cpy 1 a",
    "cpy 1 b",
    "cpy 26 d",
    "jnz c 2",
    "jnz 1 5",
    "cpy 7 c",
    "inc d",
    "dec c",
    "jnz c -2",
    "cpy a c",
    "inc a",
    "dec b",
    "jnz b -2",
    "cpy c b",
    "dec d",
    "jnz d -6",
    "cpy 16 c",
    "cpy 17 d",
    "inc a",
    "dec d",
    "jnz d -2",
    "dec c",
    "jnz c -5",
]
"""
assembunny code given as puzzle input
"""

def interpret(program):
    """
    interprets the given assembunny program (given as a list of instructions)
    and returns contents of the registers after the program terminates (as a
    dictionary)
    """
    # initialize program counter and the 4 registers
    pc = 0
    registers = {"a": 0, "b": 0, "c": 0, "d": 0}

    # interpret instructions until the program counter moves out of range of
    # the program
    while 0 <= pc < len(program):
        # XXX uncomment for debug/progress info
        #print("%d, %s" % (pc, registers))

        current_instruction = program[pc]

        if re.match("cpy", current_instruction):
            # interpret a "copy" instruction
            match = re.match("cpy (-?\d+|[a-d]) ([a-d])", current_instruction)
            target_register = match.group(2)
            if re.match("[a-d]", match.group(1)):
                # the value to copy is stored in a register
                source_register = match.group(1)
                value_to_copy = registers[source_register]
            else:
                # the value to copy is an integer constant
                value_to_copy = int(match.group(1))

            registers[target_register] = value_to_copy
            pc += 1
        elif re.match("inc", current_instruction):
            # interpret an "increment" instruction
            match = re.match("inc ([a-d])", current_instruction)
            register_to_increment = match.group(1)

            registers[register_to_increment] += 1
            pc += 1
        elif re.match("dec", current_instruction):
            # interpret a "decrement" instruction
            match = re.match("dec ([a-d])", current_instruction)
            register_to_decrement = match.group(1)

            registers[register_to_decrement] -= 1
            pc += 1
        elif re.match("jnz", current_instruction):
            # interpret a "jump-if-not-zero" instruction
            match = re.match("jnz (-?\d+|[a-d]) (-?\d+)", current_instruction)
            jump_offset = int(match.group(2))
            if re.match("[a-d]", match.group(1)):
                # the condition value is stored in a register
                condition_register = match.group(1)
                condition_value = registers[condition_register]
            else:
                # the condition value is an integer constant
                condition_value = int(match.group(1))

            if condition_value != 0:
                pc += jump_offset
            else:
                pc += 1
        else:
            # just for completeness
            raise Exception("unknown instruction %s" % current_instruction)

    return registers

def solution1():
    """
    solution to part one of the puzzle
    """
    # interpret the given code and return result value in register a
    return interpret(code)["a"]

def solution2():
    """
    solution to part two of the puzzle
    """
    # prepend an instruction to initialize register c to 1 to the original
    # code, then interpret the new code and return result value in register a
    code_with_initialization = ["cpy 1 c"] + code
    return interpret(code_with_initialization)["a"]
