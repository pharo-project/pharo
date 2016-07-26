comparing
= anObject 
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	self statements size = anObject statements size ifFalse: [ ^ false ].
	self statements with: anObject statements do: [ :first :second | first = second ifFalse: [ ^ false ] ].
	^ true