Extension { #name : #SpApplicationTest }

{ #category : #'*Spec2-Adapters-Morphic-Tests' }
SpApplicationTest >> testAdapterBindings [

	self assert: application adapterBindings class equals: SpStubAdapterBindings.
	application useBackend: #Morphic.
	self assert: application adapterBindings class equals: SpMorphicAdapterBindings
	
]
