Extension { #name : #Morph }

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> asSpAdapter [

	^ (SpBindings value 
		adapterClass: #GenericAdapter)
		morph: self
]

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> spEnsureLayoutAndAddMorph: aMorph [

	aMorph layoutFrame
		ifNil: [ aMorph layoutFrame: LayoutFrame identity].
	self addMorph: aMorph 
]
