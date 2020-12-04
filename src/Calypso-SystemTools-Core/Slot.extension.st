Extension { #name : #Slot }

{ #category : #'*Calypso-SystemTools-Core' }
Slot >> asCalypsoVariableOf: declarationClass [

	^ClyInstanceVariable on: self definedIn: self definingClass
]
