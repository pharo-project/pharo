as yet unclassified
compiledBlock: scope

	^compiledMethod 
		ifNil: [self generateBlock: CompiledMethodTrailer empty withScope: scope ]
		ifNotNil: [compiledMethod]