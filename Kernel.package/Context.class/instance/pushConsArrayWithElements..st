instruction decoding
pushConsArrayWithElements: numElements 
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do: [ :i |
		array at: i put: self pop ].
	self push: array