accessing
name

	^ (self methods isEmpty and: [ protocolOrganizer protocols isEmpty])
		ifTrue: [ self class nullCategory ]
		ifFalse: [ name ]