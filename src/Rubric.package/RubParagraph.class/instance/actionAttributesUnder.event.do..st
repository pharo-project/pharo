editing
actionAttributesUnder: aClickPoint event: anEvent do: aBlock
	|startBlock|
	
	startBlock := self characterBlockAtPoint: aClickPoint.
	(self text attributesAt: startBlock stringIndex forStyle: self textStyle) 
		select: [ :attribute| attribute rubMayActOnEvent: anEvent ]
		thenDo: [:attribute | | range boxes| 
			"find the boxes for the current attribute range"
			range := self text rangeOf: attribute startingAt: startBlock stringIndex.
			boxes := self selectionRectsFrom: (self characterBlockForIndex: range first) 
						to: (self characterBlockForIndex: range last+1).
			(boxes detect: [:each | each containsPoint: aClickPoint] ifNone: [nil])
				ifNotNil: [ aBlock cull: attribute cull: boxes ]]