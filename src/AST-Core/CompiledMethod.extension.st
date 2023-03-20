Extension { #name : #CompiledMethod }

{ #category : #'*AST-Core' }
CompiledMethod >> ast [
	"return an AST for this method. The AST is cached. see class comment of ASTCache"
	^ ASTCache at: self
]

{ #category : #'*AST-Core' }
CompiledMethod >> comments [
	"Answer a collection of strings representing the comments in the method. Return an empty collection if the method's source code does not contain a comment."

	^ self ast allComments collect: [:c| c contents]
]

{ #category : #'*AST-Core' }
CompiledMethod >> firstComment [
	"Answer a string representing the first comment in the method associated with selector. Return an empty string if the method's source code does not contain a comment."

	^ self comments ifEmpty: [ #() ] ifNotEmpty: [ :comments | comments first ]
]

{ #category : #'*AST-Core' }
CompiledMethod >> parseTree [
	"returns an AST for this method, do not cache it. (see #ast for the cached alternative)"
	| ast |
	ast := self methodClass compiler
		source: self sourceCode;
		failBlock: [^ self decompile ];
		class: self methodClass;
		parse.
	ast compilationContext compiledMethod: self.
	^ast
]
