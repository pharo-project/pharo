private
addModelIn: widget withSpecLayout: aSpec

	self model ifNil: [ ^ self ].
	
	self changed: #addModelIn:withSpecLayout: with: { widget . aSpec }