Extension { #name : #Morph }

{ #category : #'*Commander-SpecSupport' }
Morph >> createCommandContextBy: aPresenter forDropAt: anEvent [
	^aPresenter createCommandContextForSelection: #() 
]
