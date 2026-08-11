Extension { #name : 'Context' }

{ #category : '*Fuel-Core-Tests' }
Context >> assertWellMaterializedInto: aMethodContext in: aTestCase [

	aTestCase assert: self ~~ aMethodContext.
	aTestCase assert: self class == aMethodContext class.
	aTestCase assert: self temporaryVariableNames = aMethodContext temporaryVariableNames.
	aTestCase assert: self pc = aMethodContext pc.
	aTestCase assert: self stackp = aMethodContext stackPtr.
	self closure
		ifNil: [ aTestCase assert: aMethodContext closure isNil ]
		ifNotNil: [ self closure assertWellMaterializedInto: aMethodContext closure in: aTestCase ].
	aTestCase assert: self receiver = aMethodContext receiver.
	aTestCase assert: (self method isEqualRegardlessTrailerTo: aMethodContext compiledCode).
	self sender
		ifNil: [ aTestCase assert: aMethodContext sender isNil ]
		ifNotNil: [ self sender assertWellMaterializedInto: aMethodContext sender in: aTestCase ]
]

{ #category : '*Fuel-Core-Tests' }
Context >> initializeWith: aPc stackPtr: aStackPtr method: aMethod receiver: aReceiver sender: aSender [
	pc := aPc.
	stackp := aStackPtr.
	method := aMethod.
	receiver := aReceiver.
	sender := aSender.
]
