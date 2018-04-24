Extension { #name : #CompiledMethod }

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> selfMessages [
	^self ast selfMessages
]

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> superMessages [
	^self ast superMessages 
]
