"""adventofcode.com/2016/day/13"""
from collections import deque

favourite_number = 1358
"""
favourite number of the office designer, given as puzzle input
"""

initial_coordinates = (1, 1)
"""
starting position on the floor
"""

target_coordinates = (31, 39)
"""
coordinates to reach on the floor
"""

def get_neighbouring_open_space_coordinates(coordinates):
    """
    returns a list of all coordinates neighbouring the given ones at which
    there is an open space; put another way, this is the successor function for
    the graph representation of the floor/maze
    """
    def get_neighbouring_coordinates(coordinates):
        """
        returns a list of all coordinates neighbouring the given ones
        """
        x, y = coordinates

        # only coordinates with both x and y values non-negative are valid
        return [(x_, y_)
                for (x_, y_) in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                if x_ >= 0 and y_ >= 0]

    def is_open_space(coordinates):
        """
        determines whether there is open space at the given coordinates
        """
        # calculate the "magic sum" for the given coordinates
        x, y = coordinates
        sum = x * x + 3 * x + 2 * x * y + y + y * y + favourite_number

        # get number of 1 bits in the binary representation of the magic sum
        sum_in_binary = format(sum, "b")
        num_1_bits = len([bit for bit in sum_in_binary if bit == "1"])

        # the coordinates correspond to an open space if the number of 1 bits
        # is even
        return num_1_bits % 2 == 0

    return [neighbouring_coordinates
            for neighbouring_coordinates
            in get_neighbouring_coordinates(coordinates)
            if is_open_space(neighbouring_coordinates)]

def get_shortest_path_to_target(initial_coordinates, get_successors, is_target):
    """
    returns the shortest path (or one of them, if there are several) from the
    given initial coordinates to coordinates satisfying the given predicate in
    the graph specified by the given successor function
    """
    # set of coordinates that were either already visited or are in the queue
    # to be visited
    coordinates_seen = set([initial_coordinates])

    # queue of coordinates to be visited
    coordinates_to_be_visited = deque([initial_coordinates])

    # dictionary of backward parent pointers, allowing to reconstruct the path
    # the algorithm took to reach any particular coordinates
    parent_dict = {initial_coordinates: None}

    def get_traversed_path(target_coordinates):
        """
        returns the path the algorithm took to reach the given coordinates
        """
        # follow the backward pointers to get the path in reverse...
        coordinates = target_coordinates
        path = []
        while coordinates is not None:
            path.append(coordinates)
            coordinates = parent_dict[coordinates]

        # ...then reverse it
        path.reverse()
        return path

    # textbook breadth-first search stuff here; breadth-first traversal
    # guarantees that the found path will be the shortest
    while len(coordinates_to_be_visited) > 0:
        coordinates = coordinates_to_be_visited.popleft()

        if is_target(coordinates):
            return get_traversed_path(coordinates)

        for successor in get_successors(coordinates):
            if successor not in coordinates_seen:
                coordinates_seen.add(successor)
                coordinates_to_be_visited.append(successor)
                parent_dict[successor] = coordinates

    raise Exception("target coordinates not reachable") # just for completeness

def solution1():
    """
    solution to part one of the puzzle
    """
    # subtract 1 from the length of the obtained path to get the correct number
    # of steps, because the path includes both initial and target coordinates
    return len(get_shortest_path_to_target(
            initial_coordinates,
            get_neighbouring_open_space_coordinates,
            lambda coordinates: coordinates == target_coordinates)) - 1

max_distance = 50
"""
maximum distance (number of steps) from the initial coordinates to which
traversal should be limited in part two of the puzzle
"""

def get_reachable_coordinates_up_to_maximum_distance(initial_coordinates,
        get_successors, max_distance):
    """
    returns a set of coordinates reachable from the given initial coordinates
    in at most the specified number of steps in the graph specified by the
    given successor function
    """
    # set of coordinates that were either already visited or are in the queue
    # to be visited
    coordinates_seen = set([initial_coordinates])

    # queue of coordinates to be visited
    coordinates_to_be_visited = deque([initial_coordinates])

    # dictionary of backward parent pointers, allowing to reconstruct the path
    # the algorithm took to reach any particular coordinates
    parent_dict = {initial_coordinates: None}

    def get_traversed_path(target_coordinates):
        """
        returns the path the algorithm took to reach the given coordinates
        """
        # follow the backward pointers to get the path in reverse...
        coordinates = target_coordinates
        path = []
        while coordinates is not None:
            path.append(coordinates)
            coordinates = parent_dict[coordinates]

        # ...then reverse it
        path.reverse()
        return path

    # textbook breadth-first search, with the addition of stopping further
    # traversal from vertices that are at the specified maximum distance from
    # the initial vertex; because of breadth-first traversal, this guarantees
    # that the search will visit precisely those vertices reachable from the
    # initial state in at most the given maximum distance
    while len(coordinates_to_be_visited) > 0:
        coordinates = coordinates_to_be_visited.popleft()

        if len(get_traversed_path(coordinates)) - 1 == max_distance:
            continue

        for successor in get_successors(coordinates):
            if successor not in coordinates_seen:
                coordinates_seen.add(successor)
                coordinates_to_be_visited.append(successor)
                parent_dict[successor] = coordinates

    return coordinates_seen

def solution2():
    """
    solution to part two of the puzzle
    """
    return len(get_reachable_coordinates_up_to_maximum_distance(
            initial_coordinates,
            get_neighbouring_open_space_coordinates,
            max_distance))
