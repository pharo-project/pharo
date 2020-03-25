Extension { #name : #CmUICommandDisplayStrategy }

{ #category : #'*Spec2-Commander2' }
CmUICommandDisplayStrategy >> display: aCmSpecCommand in: aMenuOrGroupPresenter do: aBlock [
	aMenuOrGroupPresenter
		addItem: [ :item |
			aBlock value: item.
			item enabled: aCmSpecCommand canBeExecuted.
			item ]
]
