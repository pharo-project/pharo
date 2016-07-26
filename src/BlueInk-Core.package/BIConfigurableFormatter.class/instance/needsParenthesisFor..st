private
needsParenthesisFor: aNode
	| parent grandparent |
	aNode ifNil: [ ^ false ].
	aNode isValue
		ifFalse: [ ^ false ].
	parent := aNode parent ifNil: [ ^ false ].
	(aNode isMessage and: [ 
		parent isMessage and: [ 
		parent receiver == aNode and:[
		aNode selector isUnary not ] ] ])
		ifTrue: [ 
			grandparent := parent parent.
			(grandparent notNil and: [ grandparent isCascade ])
				ifTrue: [ ^ true ] ].
	aNode precedence < parent precedence
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ parent isAssignment ])
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ aNode isCascade ])
		ifTrue: [ ^ true ].
	aNode precedence = 0
		ifTrue: [ ^ false ].
	aNode isMessage
		ifFalse: [ ^ true ].
	aNode isUnary
		ifTrue: [ ^ false ].
	aNode isKeyword
		ifTrue: [ ^ true ].
	(parent isMessage and: [ parent receiver == aNode ])
		ifFalse: [ ^ true ].
	aNode precedence = parent precedence
		ifFalse: [ ^ true ].
	^ self useTraditionalBinaryPrecedenceForParentheses
		and: [ self precedenceOf: parent selector greaterThan: aNode selector ]