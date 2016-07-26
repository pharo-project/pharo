private validating
validateSuperclass: anObject
	anObject isBehavior ifFalse: [ 
		InvalidSuperclass 
			signal: 'Trying to build classes from non-class objects'
			for: anObject ].

	anObject isMeta ifTrue: [ 
		InvalidSuperclass 
			signal: 'Metaclasses cannot be used for building classes'
			for: anObject ].