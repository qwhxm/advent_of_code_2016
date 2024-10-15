/// adventofcode.com/2016/day/17
// NOTE run with 'g++-6 -pedantic -Wall -Wextra 17.cc -lcrypto && ./a.out'
#include <deque>
#include <functional>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include <openssl/md5.h>

/// vault passcode given as puzzle input
std::string passcode {"pgflpeqp"};

/// coordinates of a room in the maze; top-left room is (0, 0), right/left is
/// +/- on the x axis, down/up is +/- on the y axis
struct Coordinates {
    int x;
    int y;

    Coordinates(int x, int y) : x {x}, y {y} {
    }
};

/// representation of a vertex of the state-space graph of the puzzle
struct Vertex {
    /// path travelled so far through the maze
    std::string path;
    /// current coordinates in the maze (these can always be calculated from
    /// the path, but it is more efficient to store them)
    Coordinates coordinates;

    Vertex(std::string path, Coordinates coordinates) :
        path {path}, coordinates {coordinates} {
    }
};

/// returns hexadecimal representation of the MD5 hash of the given string
std::string getMD5Hash(const std::string& stringToHash) {
    unsigned char hashAsBytes[MD5_DIGEST_LENGTH];
    MD5(reinterpret_cast<const unsigned char*>(stringToHash.c_str()),
            stringToHash.length(), hashAsBytes);

    std::ostringstream hashAsHexadecimal {};
    hashAsHexadecimal << std::hex << std::setfill('0');
    for (const unsigned char byte : hashAsBytes) {
        // XXX casting byte to uint8_t here doesn't work, because uint8_t is
        //     a typedef of unsigned char, so it's not actually an integer type
        //     (what the actual fuck?!); so the plus sign is used as
        //     a workaround, because it forces some integer promotion or
        //     something (see e.g. stackoverflow.com/a/23575662)
        hashAsHexadecimal << std::setw(2) << +byte;
    }

    return hashAsHexadecimal.str();
}

/// returns coordinates that would be reached after taking a step in the given
/// direction from room with the given coordinates; does not check if the move
/// is legal, so the returned coordinates can be invalid
Coordinates getCoordinatesAfterMove(const Coordinates& coordinates,
        const char direction) {
    Coordinates coordinatesAfterMove {coordinates.x, coordinates.y};
    switch (direction) {
      case 'U':
        coordinatesAfterMove.y -= 1; break;
      case 'D':
        coordinatesAfterMove.y += 1; break;
      case 'L':
        coordinatesAfterMove.x -= 1; break;
      case 'R':
        coordinatesAfterMove.x += 1; break;
      default:
        throw std::invalid_argument("invalid direction specified");
    }
    return coordinatesAfterMove;
}

/// given the calculated hash of the path taken so far, determines whether the
/// door leading from the current room in the given direction would be open;
/// does not check if there actually is a door in the given direction
bool isDoorOpen(const char direction, const std::string& hash) {
    char relevantHashCharacter {};
    switch (direction) {
      case 'U':
        relevantHashCharacter = hash.at(0); break;
      case 'D':
        relevantHashCharacter = hash.at(1); break;
      case 'L':
        relevantHashCharacter = hash.at(2); break;
      case 'R':
        relevantHashCharacter = hash.at(3); break;
      default:
        throw std::invalid_argument("invalid direction specified");
    }
    return relevantHashCharacter >= 'b';
}

/// given the calculated hash of the path taken so far, determines whether it
/// is possible to take a step in the given direction from room with the given
/// coordinates
bool canMove(const Coordinates& coordinates, const char direction,
        const std::string& hash) {
    Coordinates coordinatesAfterMove = getCoordinatesAfterMove(coordinates,
            direction);
    bool areCoordinatesAfterMoveValid =
            coordinatesAfterMove.x >= 0 && coordinatesAfterMove.x <= 3
            && coordinatesAfterMove.y >= 0 && coordinatesAfterMove.y <= 3;

    return areCoordinatesAfterMoveValid && isDoorOpen(direction, hash);
}


/// returns a vector of successor vertices of the given vertex in the
/// state-space graph of the puzzle, when using the given passcode; since each
/// step in the maze generates new paths, different from all previous ones,
/// this function specifies an acyclic graph
std::vector<Vertex> getSuccessors(const Vertex& vertex,
        const std::string& passcode) {
    // get hash of concatenation of the passcode and path travelled so far
    std::string hash = getMD5Hash(passcode + vertex.path);

    // based on the hash, determine in which directions can a step be taken
    std::vector<Vertex> successors {};
    for (const char direction : {'U', 'D', 'L', 'R'}) {
        if (canMove(vertex.coordinates, direction, hash)) {
            successors.emplace_back(vertex.path + direction,
                    getCoordinatesAfterMove(vertex.coordinates, direction));
        }
    }
    return successors;
}

/// determines whether room with the given coordinates is the room with vault
/// access (the bottom-right room)
bool isVaultAccess(const Coordinates& coordinates) {
    return coordinates.x == 3 && coordinates.y == 3;
}

/// finds (one of) the closest vertices satisfying the given predicate in the
/// acyclic graph specified by the given successor function from the given
/// initial state
Vertex findClosestTargetVertex(const Vertex& initialVertex,
        const std::function<std::vector<Vertex>(const Vertex&)>& getSuccessors,
        const std::function<bool(const Vertex&)>& isTarget) {
    // use breadth-first search to find the closest target vertex
    // (breadth-first traversal guarantees that the first found target vertex
    // will be (one of) the closest); since the graph is acyclic, it is not
    // necessary to remember visited vertices
    std::deque<Vertex> verticesToBeVisited {initialVertex};
    while (!verticesToBeVisited.empty()) {
        Vertex& vertex = verticesToBeVisited.front();

        if (isTarget(vertex)) {
            return vertex;
        }

        for (const Vertex& successor : getSuccessors(vertex)) {
            verticesToBeVisited.push_back(successor);
        }

        verticesToBeVisited.pop_front();
    }
    throw std::runtime_error("target vertex not reachable");
}

/// solution to part one of the puzzle
std::string solution1() {
    Vertex initialVertex {"", {0, 0}};
    Vertex targetVertex = findClosestTargetVertex(initialVertex,
            std::bind(getSuccessors, std::placeholders::_1, passcode),
            [](const Vertex& vertex) {
                return isVaultAccess(vertex.coordinates);
            });
    return targetVertex.path;
}

/// finds (one of) the farthest vertices satisfying the given predicate in the
/// acyclic graph specified by the given successor function from the given
/// initial state, with the additional condition that only the first reached
/// target vertex on each path is considered (i.e. traversal does not continue
/// further from target states)
Vertex findFarthestFirstOnPathTargetVertex(const Vertex& initialVertex,
        const std::function<std::vector<Vertex>(const Vertex&)>& getSuccessors,
        const std::function<bool(const Vertex&)>& isTarget) {
    // exhaustively search the specified graph using breadth-first search, with
    // the addition of stopping further traversal from target vertices, and
    // return the farthest target vertex found; since the graph is acyclic, it
    // is not necessary to remember visited vertices
    std::deque<Vertex> verticesToBeVisited {initialVertex};
    Vertex farthestTargetVertex {"", {0, 0}};
    while (!verticesToBeVisited.empty()) {
        Vertex& vertex = verticesToBeVisited.front();

        if (isTarget(vertex)) {
            if (vertex.path.length() > farthestTargetVertex.path.length()) {
                farthestTargetVertex = vertex;
            }
        } else {
            for (const Vertex& successor : getSuccessors(vertex)) {
                verticesToBeVisited.push_back(successor);
            }
        }

        verticesToBeVisited.pop_front();
    }
    return farthestTargetVertex;
}

/// solution to part two of the puzzle
int solution2() {
    Vertex initialVertex {"", {0, 0}};
    Vertex farthestTargetVertex = findFarthestFirstOnPathTargetVertex(
            initialVertex,
            std::bind(getSuccessors, std::placeholders::_1, passcode),
            [](const Vertex& vertex) {
                return isVaultAccess(vertex.coordinates);
            });
    return farthestTargetVertex.path.length();
}

int main() {
    std::cout << "solution 1: " << solution1() << std::endl;
    std::cout << "solution 2: " << solution2() << std::endl;
    return 0;
}
