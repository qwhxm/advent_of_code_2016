"""adventofcode.com/2016/day/11"""
from collections import deque
from enum import Enum
import itertools

class FacilityObject:
    """
    a "component of interest" in the Radioisotope Testing Facility: either
    a generator or a microchip, in both cases of a specific isotope
    """
    class Type(Enum):
        GENERATOR = 1
        MICROCHIP = 2

    class Isotope(Enum):
        COBALT = 1
        POLONIUM = 2
        PROMETHIUM = 3
        RUTHENIUM = 4
        THULIUM = 5
        ELERIUM = 6
        DILITHIUM = 7

    def __init__(self, type, isotope):
        self.type = type
        self.isotope = isotope

    def __eq__(self, other):
        return isinstance(other, self.__class__) \
                and self.type == other.type \
                and self.isotope == other.isotope

    def __hash__(self):
        return hash((self.type, self.isotope))

class FacilityState:
    """
    represents state of the containment area of the Radioisotope Testing
    Facility, tracking what objects are on each of the 4 floors and on which
    floor is the elevator
    """
    def __init__(self, elevator_floor, floors):
        """
        creates a state with elevator on the given floor and the given floor
        contents (must be given as a list of 4 frozensets)
        """
        self.elevator_floor = elevator_floor
        self.floors = floors

    def __eq__(self, other):
        return isinstance(other, self.__class__) \
                and self.elevator_floor == other.elevator_floor \
                and self.floors == other.floors

    def __hash__(self):
        return hash((self.elevator_floor, tuple(self.floors)))

initial_state_1 = FacilityState(0, [
    frozenset([
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.POLONIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.THULIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.THULIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.PROMETHIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.RUTHENIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.RUTHENIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.COBALT),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.COBALT),
    ]),
    frozenset([
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.POLONIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.PROMETHIUM),
    ]),
    frozenset(),
    frozenset(),
])
"""
initial state in part one of the puzzle
"""

def get_successors(state):
    """
    returns a list of all states reachable in one step from the given state,
    excluding those in which one or more microchips would get fried; put another
    way, this is the successor function for the state-space graph of the puzzle
    """
    def move_objects(state, elevator_movement, moved_objects):
        """
        returns state obtained from the given one by moving the given objects
        one floor up or down, as specified (elevator movement must be either +1
        or -1)
        """
        new_elevator_floor = state.elevator_floor + elevator_movement
        new_floors = [None] * 4
        for index, floor in enumerate(state.floors):
            if index == state.elevator_floor:
                # remove moved objects from the old floor
                new_floors[index] = floor.difference(moved_objects)
            elif index == new_elevator_floor:
                # add moved objects to the new floor
                new_floors[index] = floor.union(moved_objects)
            else:
                new_floors[index] = floor

        return FacilityState(new_elevator_floor, new_floors)

    def are_no_chips_fried(state):
        """
        determines whether any microchips would be fried in the given state
        """
        for floor in state.floors:
            microchips = (object for object in floor
                    if object.type == FacilityObject.Type.MICROCHIP)
            generators = set([object for object in floor
                    if object.type == FacilityObject.Type.GENERATOR])

            for microchip in microchips:
                corresponding_generator = FacilityObject(
                        FacilityObject.Type.GENERATOR, microchip.isotope)
                if corresponding_generator not in generators \
                        and len(generators) > 0:
                    # if a chip is on the same floor with one or more
                    # generators, none of which is of the corresponding
                    # isotope, it gets fried
                    return False
        return True

    # get valid elevator movements from the given state (can't go down from
    # the first floor or up from the fourth floor)
    valid_elevator_movements = [elevator_movement
            for elevator_movement in [+1, -1]
            if 0 <= state.elevator_floor + elevator_movement <= 3]

    # try to move all possible combinations of 1 or 2 objects in all possible
    # directions, returning only the resulting states where no chip gets fried
    successors = []
    for elevator_movement in valid_elevator_movements:
        elevator_floor_objects = state.floors[state.elevator_floor]
        for num_moved_objects in [1, 2]:
            for moved_objects in itertools.combinations(
                    elevator_floor_objects, num_moved_objects):
                new_state = \
                        move_objects(state, elevator_movement, moved_objects)
                if are_no_chips_fried(new_state):
                    successors.append(new_state)
    return successors

def is_everything_on_fourth_floor(state):
    """
    determines whether all objects are on the fourth floor in the given state
    """
    # everything is on the fourth floor if there is nothing on the other floors
    return len(state.floors[0]) \
            == len(state.floors[1]) \
            == len(state.floors[2]) \
            == 0

def get_shortest_path_to_target(initial_state, get_successors, is_target):
    """
    returns the shortest path (or one of them, if there are several) from the
    given initial state to a state satisfying the given predicate in the graph
    specified by the given successor function
    """
    # set of states that were either already visited or are in the queue to be
    # visited
    states_seen = set([initial_state])

    # queue of states to be visited
    states_to_be_visited = deque([initial_state])

    # dictionary of backward parent pointers, allowing to reconstruct the path
    # the algorithm took to reach a particular state
    parent_dict = {initial_state: None}

    def get_traversed_path(target_state):
        """
        returns the path the algorithm took to reach the given state
        """
        # follow the backward pointers to get the path in reverse...
        state = target_state
        path = []
        while state is not None:
            path.append(state)
            state = parent_dict[state]

        # ...then reverse it
        path.reverse()
        return path

    # textbook breadth-first search stuff here; breadth-first traversal
    # guarantees that the found path will be the shortest
    while len(states_to_be_visited) > 0:
        # XXX uncomment for debug/progress info
        #print("%d, %d" % (len(states_seen), len(states_to_be_visited)))

        state = states_to_be_visited.popleft()

        if is_target(state):
            return get_traversed_path(state)

        for successor in get_successors(state):
            if successor not in states_seen:
                states_seen.add(successor)
                states_to_be_visited.append(successor)
                parent_dict[successor] = state

    raise Exception("target state not reachable") # just for completeness

def solution1():
    """
    solution to part one of the puzzle
    """
    # subtract 1 from the length of the obtained path to get the correct number
    # of steps, because the path includes both the initial and the target state
    return len(get_shortest_path_to_target(
            initial_state_1, get_successors, is_everything_on_fourth_floor)) - 1

initial_state_2 = FacilityState(0, [
    frozenset([
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.POLONIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.THULIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.THULIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.PROMETHIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.RUTHENIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.RUTHENIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.COBALT),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.COBALT),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.ELERIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.ELERIUM),
        FacilityObject(FacilityObject.Type.GENERATOR,
                FacilityObject.Isotope.DILITHIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.DILITHIUM),
    ]),
    frozenset([
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.POLONIUM),
        FacilityObject(FacilityObject.Type.MICROCHIP,
                FacilityObject.Isotope.PROMETHIUM),
    ]),
    frozenset(),
    frozenset(),
])
"""
initial state in part two of the puzzle
"""

def solution2():
    """
    solution to part two of the puzzle
    """
    # subtract 1 from the length of the obtained path to get the correct number
    # of steps, because the path includes both the initial and the target state
    return len(get_shortest_path_to_target(
            initial_state_2, get_successors, is_everything_on_fourth_floor)) - 1
