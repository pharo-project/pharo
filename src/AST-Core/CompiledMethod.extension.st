Extension { #name : #CompiledMethod }

{ #category : #'*AST-Core' }
CompiledMethod >> ast [
	"return an AST for this method. The AST is cached. see class comment of ASTCache"
	^ ASTCache at: self
]

{ #category : #'*AST-Core' }
CompiledMethod >> parseTree [
	"returns an AST for this method, do not cache it. (see #ast for the cached alternative)"
	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | 
			^ self decompile ]) methodClass: self methodClass.
]
