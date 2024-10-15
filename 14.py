"""adventofcode.com/2016/day/14"""
import hashlib

salt = "ihaygndm"
"""
salt given as puzzle input
"""

def hash_function_1(string_to_hash):
    """
    hash function used in part one of the puzzle: simple, unmodified MD5
    """
    return hashlib.md5(string_to_hash.encode("utf-8")).hexdigest()

def get_key_indices(salt, hash_function, num_keys):
    """
    returns a list of the first given number of indices that correspond to keys
    when using key-generation scheme with the given salt and hash function
    """
    def get_first_triplet_character(hash):
        """
        returns the first character that occurs three times in a row in the
        given hash, or None if there is no triplet in the hash
        """
        for i in range(len(hash) - 2):
            if hash[i] == hash[i + 1] == hash[i + 2]:
                return hash[i]
        return None

    # to avoid computing the hash for any index multiple times, store computed
    # hashes in a list containing at each index the hash for that index;
    # pre-populate the list with the first 1000 hashes
    hash_list = [hash_function(salt + str(index)) for index in range(1000)]

    # iterate through indices checking for keys until the specified number of
    # keys is found
    key_indices = []
    current_index = 0
    while len(key_indices) < num_keys:
        # extend hash list by one more hash so that the next 1000 hashes after
        # the current one are available
        hash_list.append(hash_function(salt + str(current_index + 1000)))

        # determine if the hash for the current index contains a triplet...
        triplet_character = get_first_triplet_character(
                hash_list[current_index])

        if triplet_character is not None:
            # ...and if it does, check if any of the next 1000 keys contains
            # a quintet of the same character; if it does, the current index is
            # a key index
            for hash in hash_list[current_index + 1:current_index + 1001]:
                if triplet_character * 5 in hash:
                    key_indices.append(current_index)
                    break

        current_index += 1

    return key_indices

def solution1():
    """
    solution to part one of the puzzle
    """
    # get indices for the first 64 keys, using the simple MD5 hash function,
    # and return the last (64th) one
    return get_key_indices(salt, hash_function_1, 64)[-1]

def hash_function_2(string_to_hash):
    """
    hash function used in part two of the puzzle: 2017 recursive applications
    of MD5
    """
    hash = hashlib.md5(string_to_hash.encode("utf-8")).hexdigest()
    for i in range(2016):
        hash = hashlib.md5(hash.encode("utf-8")).hexdigest()
    return hash

def solution2():
    """
    solution to part two of the puzzle
    """
    # get indices for the first 64 keys, using the key-stretching hash
    # function, and return the last (64th) one
    return get_key_indices(salt, hash_function_2, 64)[-1]
