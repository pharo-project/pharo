scan
scanPast: rangeType level: level 
	| cycle typePlusCycle |
	cycle := level <= 1
				ifTrue: [0]
				ifFalse: [level - 2 \\ 7 + 1].
	typePlusCycle := cycle = 0
				ifTrue: [rangeType]
				ifFalse: [(rangeType , cycle asString) asSymbol].
	^ self scanPast: typePlusCycle