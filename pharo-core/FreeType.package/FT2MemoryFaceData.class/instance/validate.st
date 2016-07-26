validation
validate
	self isValid ifTrue: [ ^ self ].
	bytes ifNil: [ ^ self ].
	
	self primMalloc: bytes.
	self isValid ifTrue:[ self class register: self ]