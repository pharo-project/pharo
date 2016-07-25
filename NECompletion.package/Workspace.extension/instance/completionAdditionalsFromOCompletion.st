*necompletion
completionAdditionalsFromOCompletion
	"should spend more time to get it"
	
	bindings ifNil: [ ^#() ].
	^bindings keys asArray replace: [ :each | 
		NECLocalEntry 
			contents: each
			type: #local ]