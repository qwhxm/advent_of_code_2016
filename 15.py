"""adventofcode.com/2016/day/15"""
import itertools

discs_1 = [
    (13, 10),
    (17, 15),
    (19, 17),
    (7, 1),
    (5, 0),
    (3, 1),
]
"""
specification of discs in part one of the puzzle; each tuple represents one
disc, in order, with the first element giving number of positions of the disc
and the second its starting position
"""

def get_earliest_fall_through_time(discs):
    """
    returns the first time instant at which a capsule can be dropped in
    a machine with the given disc configuration to make it through all of the
    discs without bouncing away
    XXX this is the most naive possible brute-force "algorithm"; see e.g.
        en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation for
        (much) more clever approaches
    """
    def does_capsule_fall_through(time_dropped, discs):
        """
        determines whether a capsule dropped at the given time in a machine
        with the specified disc configuration makes it through all the discs
        without bouncing away
        """
        # check if capsule dropped at the given time passes through the slot in
        # each disc, using some modular arithmetic
        for index, (num_positions, starting_position) in enumerate(discs):
            time_disc_reached = time_dropped + index + 1
            if (time_disc_reached + starting_position) % num_positions != 0:
                return False
        return True

    # check time instants one by one and return the first one at which the
    # capsule falls through
    for time in itertools.count():
        if does_capsule_fall_through(time, discs):
            return time

def solution1():
    """
    solution to part one of the puzzle
    """
    return get_earliest_fall_through_time(discs_1)

discs_2 = discs_1 + [(11, 0)]
"""
specification of discs in part two of the puzzle
"""

def solution2():
    """
    solution to part two of the puzzle
    """
    return get_earliest_fall_through_time(discs_2)
