Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Queries' }
CompiledMethod >> isOverriding [

	^(self origin superclass ifNotNil: [ :c | c lookupSelector: self selector]) notNil
]
