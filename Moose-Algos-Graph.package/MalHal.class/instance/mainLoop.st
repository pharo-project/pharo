running
mainLoop
	| cnode touchedCycle |
	[ nodesToVisit isEmpty ]
		whileFalse: [ 
			cnode := nodesToVisit removeFirst.
			cnode nextNodes
				do: [ :next | 
					touchedCycle := next markHNumber: cnode hNumber + 1.
					nodesToVisit addAll: touchedCycle ].
			nodesToVisit addAll: cnode nextNodes ]