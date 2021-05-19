Extension { #name : #BlockClosure }

{ #category : #'*Fuel-Tests-Core' }
BlockClosure >> assertWellMaterializedInto: aBlockClosure in: aTestCase [

	aTestCase assert: self ~~ aBlockClosure.
	aTestCase assert: self class == aBlockClosure class.
	aTestCase assert: self numArgs = aBlockClosure numArgs.
	aTestCase assert: self startpcOrOuterCode = aBlockClosure startpcOrOuterCode.

	outerContext
		ifNil: [ self assert: aBlockClosure outerContext isNil ]
		ifNotNil: [ self isClean
				ifTrue: [ "self assert: self receiver = aBlockClosure receiver."
					self assert: ( self method isEqualRegardlessTrailerTo: aBlockClosure method ).
					self assert: aBlockClosure outerContext sender isNil.
					self assert: aBlockClosure outerContext arguments isEmpty
					]
				ifFalse: [ outerContext assertWellMaterializedInto: aBlockClosure outerContext in: aTestCase ]
			]
]

{ #category : #'*Fuel-Tests-Core' }
BlockClosure >> fuelValueWithoutNotifications [
	SystemAnnouncer uniqueInstance suspendAllWhile: self
]

{ #category : #'*Fuel-Tests-Core' }
BlockClosure >> isEqualRegardlessTrailerTo: anObject [
	^ self = anObject
]
