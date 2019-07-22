Extension { #name : #SpAbstractTextPresenter }

{ #category : #'*Spec2-Adapters-Morphic' }
SpAbstractTextPresenter >> pendingText [
	"Return the current pending text"

	self withAdapterDo: [ :adpt | adpt isMorphicAdapter ifTrue: [ ^ adpt pendingText ] ].
	^ self getText
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpAbstractTextPresenter >> pendingText: aText [
	"Set the pending text. Do no accept it"

	self withAdapterDo: [ :adpt | adpt isMorphicAdapter ifTrue: [ ^ adpt pendingText: aText ] ].
	self text: aText
]
