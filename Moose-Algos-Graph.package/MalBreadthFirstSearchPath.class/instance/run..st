running
run: depth
	| edge path next |
	edge := fifo removeFirst.
	next := edge to.
	^ next = end
		ifTrue: [ OrderedCollection with: edge from model with: end model ]
		ifFalse:
			[ next visited
				ifFalse:
					[ fifo addAll: next nextEdges.
					next firstDepth: depth ].	"only add edges if node not already visited (avoid cycles on the path)"
			path := self run: depth + 1.	"continue recursion"
			(depth = next firstDepth) "out of stack"
				ifTrue: [
					(path first = next model) "backtrack along the path"
						ifTrue: [ path addFirst: edge from model ].	
					"reset visited for next run"
					next notVisited ].
			path ]	"stop recursion, start backtrack"