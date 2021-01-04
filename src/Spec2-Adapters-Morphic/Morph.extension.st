Extension { #name : #Morph }

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> presenter [

	^ self valueOfProperty: #presenter ifAbsent: [ nil ]
]

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> presenter: aPresenter [

	self 
		setProperty: #presenter 
		toValue: aPresenter 
]

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> spEnsureLayoutAndAddMorph: aMorph [

	aMorph layoutFrame
		ifNil: [ aMorph layoutFrame: LayoutFrame identity].
	self addMorph: aMorph 
]
