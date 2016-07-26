arithmetic
= other 
	self == other ifTrue: [ ^ true ].
	(self species = other species) ifFalse: [ ^ false ].
	^ self hasEqualTicks: other