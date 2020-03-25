Extension { #name : #CmUIDisableWhenCantBeRun }

{ #category : #'*Spec2-Commander2' }
CmUIDisableWhenCantBeRun >> display: aCmSpecCommand in: aMenuOrGroupPresenter do: aBlock [
	aMenuOrGroupPresenter
		addItem: [ :item |
			aBlock value: item.
			item enabled: aCmSpecCommand canBeExecuted.
			item ]
]
