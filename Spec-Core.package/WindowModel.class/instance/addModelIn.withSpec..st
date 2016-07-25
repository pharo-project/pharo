private
addModelIn: widget withSpec: aSpec

	self model ifNil: [ ^ self ].
	
	self changed: #addModelIn:withSpec: with: { widget . aSpec }