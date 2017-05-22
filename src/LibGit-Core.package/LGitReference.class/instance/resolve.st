operations
resolve
	| resolved |
	self isDirect 
		ifTrue: [ ^ self ].
	resolved := self class of: self repository.
	self withReturnHandlerDo: [ self reference_resolve: resolved ref: self ].
	^ resolved