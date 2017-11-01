Extension { #name : #CompiledMethod }

{ #category : #'*AST-Core' }
CompiledMethod >> ast [
	^ ASTCache at: self
]

{ #category : #'*AST-Core' }
CompiledMethod >> parseTree [
	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | 
			^ self decompile ]) methodClass: self methodClass.
]
