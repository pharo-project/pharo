Extension { #name : #LiteralVariable }

{ #category : #'*Calypso-SystemTools-Core' }
LiteralVariable >> asCalypsoVariableOf: declarationClass [
	"it is variable compatible object. Generally variable objects should have declaring class.
	Then it will be not needed and simplified"
	self isGlobalVariable ifTrue: [ ^ClyGlobalVariable on: self ]. 
	
	^ClyClassVariable on: self visibleFrom: declarationClass "scope getClass"
]
