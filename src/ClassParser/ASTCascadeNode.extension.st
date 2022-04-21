Extension { #name : #ASTCascadeNode }

{ #category : #'*ClassParser' }
ASTCascadeNode >> extractClassDefinitionFor: aFluidParser [

	aFluidParser setAdequateClassDefinitionFrom: self receiver
]
