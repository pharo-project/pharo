accessing
retrieveLast: aCollection

	^ aCollection isEmpty
		ifTrue: [ nil ]
		ifFalse: [ aCollection removeLast ]