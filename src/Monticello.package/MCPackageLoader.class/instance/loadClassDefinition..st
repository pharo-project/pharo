private
loadClassDefinition: aDefinition

	[ aDefinition isClassDefinition 
		ifTrue: [ aDefinition load ]
	] on: Error do: [ 
		errorDefinitions add: aDefinition ].