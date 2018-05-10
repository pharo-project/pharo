Extension { #name : #Context }

{ #category : #'*Fuel-Tests-Core' }
Context >> assertWellMaterializedInto: aMethodContext in: aTestCase [

	aTestCase assert: self ~~ aMethodContext. 
	aTestCase assert: (self class == aMethodContext class).
	aTestCase assert: self tempNames = aMethodContext  tempNames.
	aTestCase assert: pc = aMethodContext pc.
	aTestCase assert: stackp = aMethodContext stackPtr.
	closureOrNil isNil 
		ifTrue: [ aTestCase assert: aMethodContext closure isNil ]
		ifFalse: [ closureOrNil assertWellMaterializedInto: aMethodContext closure in: aTestCase ].
	aTestCase assert: receiver = aMethodContext receiver.
	aTestCase assert: (method isEqualRegardlessTrailerTo: aMethodContext method).
	sender isNil 
		ifTrue: [ aTestCase assert: aMethodContext sender isNil ]
		ifFalse: [ sender assertWellMaterializedInto: aMethodContext sender in: aTestCase ]
]

{ #category : #'*Fuel-Tests-Core' }
Context >> initializeWith: aPc stackPtr: aStackPtr method: aMethod receiver: aReceiver sender: aSender [
	pc := aPc.
	stackp := aStackPtr.
	method := aMethod.
	receiver := aReceiver.
	sender := aSender.
	
]
