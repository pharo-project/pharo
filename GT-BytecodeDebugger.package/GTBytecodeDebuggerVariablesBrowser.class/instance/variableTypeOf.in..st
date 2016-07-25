as yet unclassified
variableTypeOf: aString in: aContext
	|variableName|
	
	variableName := (aString includes: $[)
			ifTrue: [ (aString copyFrom: (aString indexOf: $[) +1 to: (aString indexOf: $])-1) trim  ]
			ifFalse: [ aString ].
	^ super 
		variableTypeOf: variableName
		in: aContext