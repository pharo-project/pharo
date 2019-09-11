Extension { #name : #OCUndeclaredVariable }

{ #category : #'*Calypso-SystemTools-Core' }
OCUndeclaredVariable >> asCalypsoVariableOf: declarationClass [
	
	"Undeclared variable behaves like a global. It is shared in all places in system"	
	^ClyGlobalVariable on: self assoc
]
