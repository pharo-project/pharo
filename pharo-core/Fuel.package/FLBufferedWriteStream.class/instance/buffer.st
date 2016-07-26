private
buffer
	buffer ifNil: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer