private
buffer
	buffer isNil 
		ifTrue: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer