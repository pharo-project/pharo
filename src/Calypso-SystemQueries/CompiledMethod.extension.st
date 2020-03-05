Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemQueries' }
CompiledMethod >> calypsoEnvironmentType [
	^ClyMethod
]

{ #category : #'*Calypso-SystemQueries' }
CompiledMethod >> isBigLiteralMethod [

	^ self isLiteralMethod 
		and: [ self literals anySatisfy: [ :literal | 
				literal class isVariable and: [ literal size > 500 ] ] ]
]

{ #category : #'*Calypso-SystemQueries' }
CompiledMethod >> isLiteralMethod [

	^ self bytecode = self encoderClass literalMethodBytecodes
]
