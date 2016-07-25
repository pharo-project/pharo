comparing
= anObject 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages with: anObject messages do: [:first :second | first = second ifFalse: [^false]].
	^true