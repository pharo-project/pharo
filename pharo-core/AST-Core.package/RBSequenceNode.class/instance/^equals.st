comparing
= anObject 
	"Can't send = to the temporaries and statements collection since they might change from arrays to OCs"

	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self temporaries size = anObject temporaries size ifFalse: [^false].
	self temporaries
		with: anObject temporaries
		do: [:first :second | first = second ifFalse: [^false]].
	self statements size = anObject statements size ifFalse: [^false].
	self statements
		with: anObject statements
		do: [:first :second | first = second ifFalse: [^false]].
	^true