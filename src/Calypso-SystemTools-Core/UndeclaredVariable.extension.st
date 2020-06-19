Extension { #name : #UndeclaredVariable }

{ #category : #'*Calypso-SystemTools-Core' }
UndeclaredVariable >> asCalypsoVariableOf: declarationClass [
	
	"Undeclared variable behaves like a global. It is shared in all places in system"	
	^ClyGlobalVariable on: self
]
