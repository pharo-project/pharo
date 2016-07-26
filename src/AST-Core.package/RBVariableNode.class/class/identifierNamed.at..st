instance creation
identifierNamed: anIdentifierName at: aPosition
	
	anIdentifierName = 'self'
		ifTrue: [ ^ RBSelfNode named: anIdentifierName start: aPosition ].
	anIdentifierName = 'thisContext'
		ifTrue: [ ^ RBThisContextNode named: anIdentifierName start: aPosition ].
	anIdentifierName = 'super'
		ifTrue: [ ^ RBSuperNode named: anIdentifierName start: aPosition ].
	^ self named: anIdentifierName start: aPosition.