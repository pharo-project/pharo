Extension { #name : #SpAbstractListPresenter }

{ #category : #'*Spec2-Adapters-Morphic' }
SpAbstractListPresenter >> iconFor: anItem [
	"This is needed for the Morphic backend because all of them share the same datasource asking for this method. For lists without icons it will return nil."

	^ nil
]
