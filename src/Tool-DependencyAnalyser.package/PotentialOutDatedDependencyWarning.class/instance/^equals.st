comparing
= anObject
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ (package == anObject package) and: [ dependency == anObject dependency ]