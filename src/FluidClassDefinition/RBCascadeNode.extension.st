Extension { #name : #RBCascadeNode }

{ #category : #'*FluidClassDefinition' }
RBCascadeNode >> extractClassDefinitionFor: aFluidParser [

	aFluidParser setAdequateClassDefinitionFrom: self receiver
]
