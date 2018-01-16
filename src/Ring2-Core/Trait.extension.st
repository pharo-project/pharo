Extension { #name : #Trait }

{ #category : #'*Ring2-Core' }
Trait >> asRing2MinimalCommentDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: ('classComment:', self name) asSymbol ifAbsentRegister: [
		RG2CommentDefinition parent: (self asRing2MinimalDefinitionIn: aRG2Environment).
	]. 
	
]

{ #category : #'*Ring2-Core' }
Trait >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2TraitDefinition named: self name parent: aRG2Environment]

]
