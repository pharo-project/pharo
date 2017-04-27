reading
entryPositionsUpTo: endPosition

	| positions |
	positions := OrderedCollection new.
	
	self entryPositionsDo: [ :position |
		position > endPosition ifTrue: [ ^positions ].
		positions add: position ].
	
	^ positions