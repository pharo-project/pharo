Extension { #name : #CmUIHideWhenCantBeRun }

{ #category : #'*Spec2-Commander2' }
CmUIHideWhenCantBeRun >> display: aCmSpecCommand in: aMenuOrGroupPresenter do: aBlock [
	aCmSpecCommand canBeExecuted "If can not be run, stop because we want to hide the command."
		ifFalse: [ ^ self ].
	aMenuOrGroupPresenter
		addItem: [ :item |
			aBlock value: item.
			item ]
]
