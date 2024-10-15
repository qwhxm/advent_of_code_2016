"""adventofcode.com/2016/day/16"""

initial_state = "10111100110001111"
"""
initial state for random data generation given as puzzle input
"""

def generate_random_data(length, initial_state):
    """
    generates random-but-not-suspicious data of the given length, using
    the given initial state
    XXX this is the most naive possible implementation, (much) more clever
        approaches are possible - see e.g. redd.it/5ititq
    """
    random_data = initial_state

    # repeatedly extend the generated data until required length is reached
    while len(random_data) < length:
        # in one iteration step, take the data generated so far and reverse
        # it...
        new_random_data = reversed(random_data)
        # ...replace 0s with 1s and 1s with 0s...
        new_random_data = "".join(["1" if character == "0" else "0"
                for character in new_random_data])
        # ...and append the result to the original data, with a 0 in between
        random_data = random_data + "0" + new_random_data

    # return only the specified number of characters
    return random_data[:length]

def calculate_checksum(data):
    """
    calculates checksum of the given data
    XXX this is the most naive possible implementation, (much) more clever
        approaches are possible - see e.g. redd.it/5ititq
    """
    def get_checksum_string(string):
        """
        returns the checksum string for the given input string (performing only
        one step of the checksum generation process); the given string must
        have even length
        """
        checksum_string = ""
        for i in range(0, len(string), 2):
            if string[i] == string[i + 1]:
                checksum_string += "1"
            else:
                checksum_string += "0"
        return checksum_string

    # recursively apply checksum string calculation until a checksum with odd
    # length is generated
    checksum = get_checksum_string(data)
    while len(checksum) % 2 == 0:
        checksum = get_checksum_string(checksum)
    return checksum

def solution1():
    """
    solution to part one of the puzzle
    """
    # return checksum for random data of length 272 generated using the given
    # initial state
    return calculate_checksum(generate_random_data(272, initial_state))

def solution2():
    """
    solution to part two of the puzzle
    """
    # return checksum for random data of length 35651584 generated using
    # the given initial state
    return calculate_checksum(generate_random_data(35651584, initial_state))
