comparing
= anObject 
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body = anObject body ] ])
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		first = second 
			ifFalse: [ ^ false ] ].
	self pragmas with: anObject pragmas do: [ :first :second |
		first = second 
			ifFalse: [ ^ false ] ].
	^ true