Extension { #name : #CompiledMethod }

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> selfMessages [
	^(self ast sendNodes select: [ :node | node isSelfSend ] thenCollect: [ :node | node selector ]) asSet
	
]

{ #category : #'*Refactoring-Critics' }
CompiledMethod >> superMessages [
	^(self ast sendNodes select: [ :node | node isSuperSend ] thenCollect: [ :node | node selector ]) asSet
]
