comparing
= anObject 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	(self receiver = anObject receiver 
		and: [self selector = anObject selector]) ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | first = second ifFalse: [^false]].
	^true