validation
validate
	self isValid ifTrue: [ ^ self ].
	bytes ifNil: [ ^ self  ].
	
	self primCopyToExternalMemory: bytes.
	self isValid ifTrue: [ self class register: self ]