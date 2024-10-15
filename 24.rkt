;;; adventofcode.com/2016/day/24
#lang racket

(require data/queue)
(require srfi/25) ; for multi-dimensional arrays

;; duct map given as puzzle input
(define duct-map
  (apply array
         (shape 0 37 0 181)
         (flatten
           (map string->list
                '("#####################################################################################################################################################################################"
                  "#.....#.........#.#...#.....#.............#.......#.....#.....#...........#...#.........#.#.#.....#.......#...............#..........3#.#.#.....#.......#...#.....#...#.#.#.....#...#"
                  "#.###.#.#.###.#.#.#.#.#.#.#.#.#.#####.#####.#.###.#.#.#######.###.#.#######.#.#.#.#.#.#.#.#.#.#####.#.#.###.#######.#.###.###.#.#.#.#.#.#.#.#.#.#.#.#.#####.#.###.#.#.#.#.###.#.###.#"
                  "#.......#.#...#...#.#...#...#.#...#...#.#...#.....#...#.#.....#.....#.....#.......#...#...#.................#.#.............#...#.....#.........#...#...#.#...#...#.....#.......#...#"
                  "#.#.#.###.#.#.###.#.#.#.#.###.#.###.###.#.#.#.#######.#.#####.#.#.#####.#.#.#.#####.#.###.#.#####.#####.#.###.###.###.#####.#.#.#.#.#.#.#.#.#.#.###.###.#.#.#.#.#####.#.#.#.#.#.#####"
                  "#..1#.......#...........#...#.........#.#.....#...#.#...#.........#...#...#...#.....#.#...#.#.#.....#...#.#.#...#.......#.........#.......#...#.#...#.....#.#.....#...#...#..2#.....#"
                  "#.#####.###.#.#.#.###.###.###.#####.#.#.#.#.###.#.#.#.#.#####.###.#.#.#####.#.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#####.###.#.#.#####.###.###.#.#.#.###.#.#####.#.#.#.###.#.#.#.#.#"
                  "#...#.............#.#...#.#...#...#.#.#...#...#.............#.#.....#.........#.........#.#...#.#.#.#...#.......#.#.......#...#...#.#.......#...#.#.....#.........#.#.#.#.........#.#"
                  "#.#.#.###.###.#.#.#.#.#.#.#.#.#.#.#.###.###.###.#.#####.#.#.#.###.#.#.#.#####.#.#.###.#.#.#.#.#.###.#.#.#.#####.#####.###.#.#.#.#.#.#.#######.#.#.#.#.#.#.#######.#.#.#.#.###.#.#.#.#"
                  "#.......#...#.....#.....#.......#...#.....#.#.#.........#.......#.#.....#...#.#...#...#.#.....#...#.#...........#.........#...#.#.#...#.#.....#.....#.....#...........#...#.......#.#"
                  "#####.###.#.###########.###.#.###.###.###.#.#.#.###.#.###.###.#.#.#.#####.#.#.#.#########.#####.#.#.###.#.#.#.#.#.###.#.#.#.###.#.#####.#.#.#.#.#.#.#.#####.###.#####.###.#.#.#.#.#.#"
                  "#...#...#.......#.....#.....#.....#.....#.......#.#.#.....#...........#.....#.#.#.#.......#.....#.......#...........#.#...#...#.#.......#...#.....#...#.#...#.#...#...#.....#.....#.#"
                  "#.#.###.#.###.#.#####.#.#.#.#.#.#.###.#.###.###.#.#.#.###.#.#.#.###.#.#.###.#.#.#.#.#.#.#.###.#.#.#######.###.#######.#.###.###.#.###.#.#.#.###.###.#.#.#.#.#.#.#.#.###.#.#.###.#.#.#"
                  "#.....#...#.........#...#...#.#.#.........#.#.#...#.#...#.#...#.#.........#.....#.#...#.#...#...#.......#.....#...#...#.#.....#.......#.#...#...#.........#.#...#.#.........#.#...#.#"
                  "#.###.###########.#.###.#.#.#.#####.#.#.#.###.#.#.#.#####.#.###.#.#.#######.#####.#.#.#.#.###.#.#.###.#.#.#####.###.#.#.#.#.#.#########.###.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#####.#"
                  "#.#.................#.............#...#...#.#.#.#...#...#...#.....#.......#.#.#...#...........#.........#.......#.........#...#...#...#.........#.#...#...#.........#.........#...#.#"
                  "#.#.#.#####.#######.###.###.###.#.#######.#.#.###.###.#.#.#.#####.#####.###.#.#.#.#.###.#.###.#.#.#####.#.#.#.#.#.#.###.#.#.#.#.#.#.#.#.###.###.#######.###.#.#.#.#.#.#.###.#.#.#.#.#"
                  "#...#.#.#...................#0............#...........#.#.....#.#.....#.#.........#.....#.......#.......#.....#.......#.#...#.......#.#.#...#.............#...#.....#.#.......#...#6#"
                  "#.#.#.#.#.###.#.#.#.#.#####.###.#.#.#####.#####.###.#.###.###.#.#.#.#.#.#.#####.#.#.#.#.#.#####.#.###.#.#####.#.#####.#.#.#.#.#####.#.#.#.#.#.#.#.#########.#.###.###.#######.#.#.###"
                  "#.#...#.#.......#.#.#.#.....#...#...#.#...#...#.#...#.........#...#...#...#.....#.....#.....#...#.....#.......#.....#...#...#.#.....#.#...#.#.#.#.#.......#...#.......#...#...#...#.#"
                  "#.###.#.###.#.#.#.#.#.###.#.#.#.###.#.###.#.#.#.#.###.#.#.#.#.#.#.#####.#.#####.#.#####.#.#.#.###.#.#############.###.###.###.###########.#.###.#.#.#.###.#.###.###.#.#.#.#.#.#.###.#"
                  "#.....#.#...#...#...#...#.#.#.........#.....#...#...#.#.....#...#.#...........#.#.......#...#.#.......#.#...#.........#...#...#.#.#.....#...#.#.#.#.......#...........#...#.#.......#"
                  "#.#.#####.#.###########.#.#.#.#############.#.#.#.#.#######.#######.###.#.###.###.###.#######.#.###.#.#.#.#.#######.###.###.###.#.#.#.#.#.#.#.#.#.#.###.#.#######.###.###.#.#.#.#####"
                  "#...#.#.......#.................#.#.........#.....#.#.#.....#...#.....#.......#...#...#.......#.#...#.#.#...#...........#.#.#.....#.#.........#...#.#...........#...#.....#...#.#...#"
                  "###.#.#.#.###.#.#.#.#.#.#.###.#.#.#.#.#.#.###.#.#.#.#.#.#.#.###.#.###.#.###.###.#.#####.#####.###.#.#.#.#######.#.#.#.#.#.#.#.###.#.#.###.#.#.###.#.#.#####.#.#.#.###.#.#.###.#.#.#.#"
                  "#...#...#.....#.#.#...#...#...#...#.............#.....#...#.#.#...#.............#.#.............#...#.#.#...#.#.#...#.#...#.#.#.......#.#.......#...#.#.....#...#...#.#...#.#...#...#"
                  "#.#.#.#.#.#####.#.#.#.#.#.#.#######.###.#######.#.###.#.###.#.#.#.###.#.#.###.#.#.#.#.#.#.#.###.###.#.###.###.###.###.###.###.###.#####.#######.###.#.###.#.#.###.#.#.#.###.###.#.#.#"
                  "#...#.#.#.......#.#.#...#...........#.........#.#.#...#.#.#.#.#.#.............#...#...#...#.....#.......#...#.#...#...#...#...#.........#...#...#.....#.#.....#.#.#...#...#.#...#...#"
                  "###.#.#.#.###.###.###.#.#####.#.#.#.#.#.#####.###.#.###.#.#.#.#.###.#.###.###.###.#.#.#.###.###.###.###.###.###.#.#.###.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#.###.###.#"
                  "#...#...#.#...#...#.#.#.......#...#...#.#.......#.......#.#.....#.........#...........#.....#...#...#.......#...........#...#...#.#.#...#.......#...#.....#.....#.#....5#.....#.....#"
                  "#.#.#.#####.#.#.#.#.###.#.#.#.###.#.#.###.#####.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.###############.#.###.#.#.#.###.###.#.#.#.#.###.#.#.###.#####.#.#.#####.###.###.#.#.#.###.#.#.#.#.#"
                  "#.........#.#...#.....#...#.#.#...#.....#...#.....#.....#...#.#.#...#.#.....#...#.............#...#.#.....#.#.....#...........#...#.............#...#...#.#...#...#.#.......#...#...#"
                  "#.#.#.#.#.#.###.#####.###.#.#.#.#.#.###.#.###.#.###.#.#.#.#.#.###.#.#.#.#####.#.#####.#####.###.#.#.#.#############.#####.#.###.#.###.#.#.#.#.#####.#.#.#.#.#.#.#.#.#.#.#.#.#.#######"
                  "#4#.#.....#.#.....#...#...#...#...#...#.#.#...#...#...#.#.....#...#...#.........#...#.#.....#...#.#...#.#.....#.#.#...#...#.#...#.#.......#.#.......#...#.......#.#.#.#.#.........#.#"
                  "#####.#.###.###.###.#####.###.#.#.###.#.#.#.#.#.#.#.#.#####.#.#.#.#.###.#.#.#.#.#.#.#.#.#.###.#.#.###.#.#.#.#.#.#.###.#.#.###.#.#.###.#.#.#.###.###.#.#.#.#.#####.#.###.#.#####.###.#"
                  "#.......#...#...#...#.#.#.........#...#.#7#.#...#...#.......#.#.#.#.....#.#.....#.....#.....#...#.#.#.#...........#...#.....#.............#...............#.....#.........#...#.....#"
                  "#####################################################################################################################################################################################")))))

;; returns coordinates of the given waypoint in the given duct map
;; (top-left room is #(0 0), down/up is +/- on the x axis, right/left is
;; +/- on the y axis)
(define (coordinates-of duct-map waypoint)
  (define coordinates-list
    (map list->vector (cartesian-product (range (array-end duct-map 0))
                                         (range (array-end duct-map 1)))))
  (findf (lambda (coordinates)
           (equal? (array-ref duct-map coordinates) waypoint))
         coordinates-list))

;; returns a list of all coordinates in the given duct map neighbouring
;; the given ones at which there is an open space; put another way, this is
;; the successor function for graph representation of the duct map
(define (neighbouring-open-space-coordinates duct-map coordinates)
  (define (neighbouring-coordinates coordinates)
    (define x (vector-ref coordinates 0))
    (define y (vector-ref coordinates 1))
    (list (vector (+ x 1) y)
          (vector (- x 1) y)
          (vector x (+ y 1))
          (vector x (- y 1))))

    (define (in-map? coordinates)
      (define x (vector-ref coordinates 0))
      (define y (vector-ref coordinates 1))
      (and (and (>= x 0) (< x (array-end duct-map 0)))
           (and (>= y 0) (< y (array-end duct-map 1)))))

    (define (open-space? coordinates)
      (not (equal? (array-ref duct-map coordinates) #\#)))

    (filter (lambda (coordinates)
              (and (in-map? coordinates) (open-space? coordinates)))
            (neighbouring-coordinates coordinates)))

;; returns a vector of minimum distances from the given initial vertex to the
;; given target vertices in the graph specified by the given successor function
;; (the returned vector contains at index i the distance to the i-th target)
(define (distances-to-targets initial-vertex successors targets)
  ;; returns the path the traversal took to reach the given vertex
  (define (traversed-path vertex)
    (define predecessor (hash-ref predecessor-references vertex))
    (cond
      [(equal? predecessor #f)
       ; vertex is the initial vertex
       (list vertex)]
      [else
       (append (traversed-path predecessor) (list vertex))]))

  ; use breadth-first search to find shortest paths/minimum distances to all
  ; specified target vertices
  (define seen-vertices (mutable-set))
  (set-add! seen-vertices initial-vertex)
  (define to-be-visited-vertices (make-queue))
  (enqueue! to-be-visited-vertices initial-vertex)
  (define predecessor-references (make-hash))
  (hash-set! predecessor-references initial-vertex #f)

  (define distances-vector (make-vector (length targets)))
  (define num_found_distances 0)
  (for ([_ (in-naturals)])
    #:break (= num_found_distances (length targets))

    (define vertex (dequeue! to-be-visited-vertices))
    (define successor-vertices (successors vertex))
    (define unseen-successor-vertices
      (filter-not ((curry set-member?) seen-vertices) successor-vertices))
    (for ([successor-vertex unseen-successor-vertices])
      (set-add! seen-vertices successor-vertex)
      (enqueue! to-be-visited-vertices successor-vertex)
      (hash-set! predecessor-references successor-vertex vertex))

    ; check if vertex is a target vertex
    (define i (index-of targets vertex))
    (cond
      [(not (equal? i #f))
       ; vertex is the i-th target vertex, so record its distance
       (define distance (- (length (traversed-path vertex)) 1))
       (vector-set! distances-vector i distance)
       (set! num_found_distances (+ num_found_distances 1))]))
  distances-vector)

;; returns an array of minimum pairwise distances between the given vertices
;; in the graph specified by the given successor function (the returned array
;; contains at index #(i j) the distance between the i-th and the j-th target)
; XXX unnecessarily determines each distance twice, could be rewritten not to
(define (distances-between-targets successors targets)
  ; call distances-to-targets with each target as initial vertex and store
  ; the results in an array
  (define distances-matrix
    (make-array (shape 0 (length targets) 0 (length targets))))
  (for ([i (in-range (length targets))])
    (define target-i (list-ref targets i))
    (define distances-vector-i
      (distances-to-targets target-i successors targets))
    (for ([j (in-range (vector-length distances-vector-i))])
      (array-set! distances-matrix
                  (vector i j)
                  (vector-ref distances-vector-i j))))
  distances-matrix)

;; returns an array of minimum pairwise distances between the given waypoints
;; in the given duct map (the returned array contains at index #(i j)
;; the distance between the i-th and the j-th waypoint)
(define (distances-between-waypoints duct-map waypoints)
  (define successors ((curry neighbouring-open-space-coordinates) duct-map))
  (define targets (map ((curry coordinates-of) duct-map) waypoints))
  (distances-between-targets successors targets))

;; returns the length of the given path in a complete graph specified by
;; the given distance matrix (vertices are represented as indices
;; to the matrix)
(define (path-length distances-matrix path)
  (for/fold ([path-length 0])
            ([i (in-range (- (length path) 1))])
    (define vertex (list-ref path i))
    (define next-vertex (list-ref path (+ i 1)))
    (define distance-to-next-vertex
      (array-ref distances-matrix (vector vertex next-vertex)))
    (+ path-length distance-to-next-vertex)))

;; returns the length of the shortest path beginning with the given vertex
;; and visiting all other vertices in a complete graph specified by the given
;; distance matrix
(define (shortest-hamiltonian-path distances-matrix initial-vertex)
  (define vertices (range (array-end distances-matrix 0)))
  (define vertices-to-visit (remove initial-vertex vertices))
  (define possible-paths
    (map (lambda (permutation)
           (append (list initial-vertex) permutation))
         (permutations vertices-to-visit)))
  (argmin ((curry path-length) distances-matrix) possible-paths))

;; solution to part one of the puzzle
(define (solution1)
  ; prepare a distance matrix specifying a complete graph where vertices are
  ; waypoints in the given duct map and edge weights are distances between them
  (define waypoints '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
  (define distances-matrix (distances-between-waypoints duct-map waypoints))

  ; in this graph, find the length of the shortest Hamiltonian path beginning
  ; with vertex 0 (corresponds to waypoint #\0)
  (define shortest-path (shortest-hamiltonian-path distances-matrix 0))
  (path-length distances-matrix shortest-path))

;; returns the length of the shortest path beginning and ending with the given
;; vertex and visiting all other vertices in between in a complete graph
;; specified by the given distance matrix
; XXX a cycle doesn't really need an initial vertex
(define (shortest-hamiltonian-cycle distances-matrix initial-vertex)
  (define vertices (range (array-end distances-matrix 0)))
  (define vertices-to-visit (remove initial-vertex vertices))
  (define possible-paths
    (map (lambda (permutation)
           (append (list initial-vertex) permutation (list initial-vertex)))
         (permutations vertices-to-visit)))
  (argmin ((curry path-length) distances-matrix) possible-paths))

;; solution to part two of the puzzle
(define (solution2)
  ; prepare a distance matrix specifying a complete graph where vertices are
  ; waypoints in the given duct map and edge weights are distances between them
  (define waypoints '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
  (define distances-matrix (distances-between-waypoints duct-map waypoints))

  ; in this graph, find the length of the shortest Hamiltonian cycle
  (define shortest-path (shortest-hamiltonian-cycle distances-matrix 0))
  (path-length distances-matrix shortest-path))
