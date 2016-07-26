comparing
= other
	self class = other class ifFalse: [ ^ false ].
	^ self headers = other headers and: [ self entity = other entity ]