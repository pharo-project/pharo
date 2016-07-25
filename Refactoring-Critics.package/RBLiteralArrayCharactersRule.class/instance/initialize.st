initialization
initialize
	super initialize.
	self matcher 
		matches: '`#literal'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(node value class == Array and: [ self isArrayOfCharacters: node value ]) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]