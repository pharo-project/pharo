*UnifiedFFI
readString
	self isNull ifTrue: [ ^ nil ].
	^ self fromCString