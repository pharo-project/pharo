comparing
= other
	self class = other class ifFalse: [ ^ false ].
	^ self headers = other headers