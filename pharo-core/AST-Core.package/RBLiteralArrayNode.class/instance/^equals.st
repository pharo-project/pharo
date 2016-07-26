comparing
= anObject 
	super = anObject ifFalse: [^false].
	self isForByteArray = anObject isForByteArray ifFalse: [^false].
	self contents size = anObject contents size ifFalse: [^false].
	self contents
		with: anObject contents
		do: [:first :second | first = second ifFalse: [^false]].
	^true