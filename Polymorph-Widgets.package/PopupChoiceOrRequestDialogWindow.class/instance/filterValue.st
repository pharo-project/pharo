accessing
filterValue

	^filter isEmpty
		ifTrue: [ nil ]
		ifFalse: [ filter ]