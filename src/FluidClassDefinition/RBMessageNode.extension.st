Extension { #name : #RBMessageNode }

{ #category : #'*FluidClassDefinition' }
RBMessageNode >> extractClassDefinitionFor: aFluidParser [

	aFluidParser setAdequateClassDefinitionFrom: self receiver
]
