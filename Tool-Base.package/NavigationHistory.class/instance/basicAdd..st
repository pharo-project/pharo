protocol
basicAdd: anEntry
	
	( storage isEmpty not and: [ storage last = anEntry ] ) ifTrue: [ ^ self ].
	anEntry = self current key ifTrue: [ ^ self ].
	self checkSize.
	index := index +1.
	index > storage size
		ifTrue: [ storage addLast:  (anEntry -> DateAndTime now)]
		ifFalse: [ 
			storage := storage copyFrom: 1 to: index.
			storage at: index put: (anEntry -> DateAndTime now)]