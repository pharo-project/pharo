assignments
typeFor: anObject 
	anObject isString
		ifTrue: [ ^ String ].
	anObject isInteger
		ifTrue: [ ^ Integer ].
	^ (anObject == true or: [ anObject == false ])
		ifTrue: [ Boolean ]
		ifFalse: [ anObject class ]