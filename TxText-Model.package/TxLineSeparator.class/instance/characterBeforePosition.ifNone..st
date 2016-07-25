accessing
characterBeforePosition: spanPosition ifNone: aBlock

	"Answer character which is right before the position, located in receiver
	or evaluate a block if there's none"
	
	(spanPosition = 1)
		ifTrue: [ ^ Character cr ].
		
	^ aBlock value