Extension { #name : #OCSlotVariable }

{ #category : #'*Calypso-SystemTools-Core' }
OCSlotVariable >> asCalypsoVariableOf: declarationClass [

	^ClyInstanceVariable on: slot visibleFrom: scope outerScope getClass
]
