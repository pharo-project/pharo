Extension { #name : #RBCascadeNode }

{ #category : #'*ClassParser' }
RBCascadeNode >> extractClassDefinitionFor: aFluidParser [

	aFluidParser setAdequateClassDefinitionFrom: self receiver
]
