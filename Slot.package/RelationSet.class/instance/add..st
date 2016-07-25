adding
add: anObject

	(self includes: anObject)
		ifTrue: [ ^anObject ].
		
	slot add: anObject to: owner.
	^set add: anObject