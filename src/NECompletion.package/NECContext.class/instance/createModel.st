accessing
createModel
	| modelClass |
	
	self receiverClass
		ifNotNil: [^ NECTypedModel class: receiverClass].

	(controller workspace isNil and: [ theClass notNil ])
		ifTrue: [ self isStartOfMethodDefinition
				ifTrue: [ NECPreferences overrideModel
						ifTrue: [ ^ NECOverrideModel class: theClass ]
						ifFalse: [ ^ NECEmptyModel new ] ] ].
							
	self completionTokenIsLiteralOrComment ifTrue:[ ^ NECEmptyModel new].
				
	modelClass := NECUntypedModel.		
	(controller model respondsTo: #modelClass) 
		ifTrue: [
			controller model modelClass ifNotNil: [:class | modelClass :=  class]].	
			
	^ modelClass
		class: theClass
		temporaries: self temporaries
		additionals: controller additionals
		variables: variables
		selectors: selectors