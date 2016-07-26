geometry
handleBoundsChange: aBlock
	| oldBounds |
	oldBounds := self bounds copy.
	aBlock value.
	oldBounds topLeft ~= self bounds topLeft
		ifTrue: [ self announce: ((RubPositionChanged morph: self) previousBounds: oldBounds)].
	oldBounds extent ~= self bounds extent
		ifTrue: [ 
			self scrollPane ifNotNil: [ :sp | sp textAreaExtentChanged ].
			self announce: ((RubExtentChanged morph: self) previousBounds: oldBounds) ].
