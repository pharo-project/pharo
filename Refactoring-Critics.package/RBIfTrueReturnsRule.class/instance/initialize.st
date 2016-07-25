initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'| `@temps | ``@.Statements. ``@object ifTrue: [^``@value1]. ^``@value2' 
			'| `@temps | ``@.Statements. ``@object ifFalse: [^``@value1]. ^``@value2' )
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| condition |
					condition := (node statements at: node statements size - 1) arguments first 
						body statements 
						last value.	"``@value1"
					((condition isLiteralNode
						and: [ #(true false) includes: condition value ]) 
						or: [ 
							condition := 	node statements last value.
							condition isLiteralNode and: [ #(true false) includes: condition value ] ]) 
								ifTrue: [ node ]
								ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]