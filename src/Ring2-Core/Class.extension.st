Extension { #name : #Class }

{ #category : #'*Ring2-Core' }
Class >> asRing2MinimalCommentDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: ('classComment:', self name) asSymbol ifAbsentRegister: [
		RG2CommentDefinition parent: (self asRing2MinimalDefinitionIn: aRG2Environment).
	]. 
	
]

{ #category : #'*Ring2-Core' }
Class >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2ClassDefinition named: self name asSymbol parent: aRG2Environment]. 
	
]
