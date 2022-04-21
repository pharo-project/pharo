Extension { #name : #ASTMessageNode }

{ #category : #'*ClassParser' }
ASTMessageNode >> extractClassDefinitionFor: aFluidParser [

	aFluidParser setAdequateClassDefinitionFrom: self
]
