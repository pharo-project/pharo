comparing
= other
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name 
			and: [ method = other method
			and: [ startpc = other startpc ] ] ]