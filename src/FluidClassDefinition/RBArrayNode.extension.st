Extension { #name : #RBArrayNode }

{ #category : #'*FluidClassDefinition' }
RBArrayNode >> isEmpty [
	^ statements isEmpty 
]

{ #category : #'*FluidClassDefinition' }
RBArrayNode >> lastIsReturn [
	statements isEmpty ifTrue:[ ^false ].
	^statements last lastIsReturn
]
