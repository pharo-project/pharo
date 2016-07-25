private
parse: aText isMethod: isMethod
	|root|
	
	isMethod 
		ifTrue: [
			[root := RBParser parseFaultyMethod: aText asString.
			root methodNode methodClass: classOrMetaClass.
			^root] 
				on: Error 
				do: [^RBParser parseFaultyExpression: aText asString]]
		ifFalse:[ ^RBParser parseFaultyExpression: aText asString ].