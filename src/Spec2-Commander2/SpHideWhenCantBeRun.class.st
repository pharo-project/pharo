"
I implement the fact that the button corresponding to a command is hidden when it can not be run.
"
Class {
	#name : #SpHideWhenCantBeRun,
	#superclass : #SpCommandDisplayStrategy,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #displaying }
SpHideWhenCantBeRun >> display: aCmSpecCommand in: aMenuOrGroupPresenter do: aBlock [
	aCmSpecCommand canBeRun "If can not be run, stop because we want to hide the command."
		ifFalse: [ ^ self ].
	aMenuOrGroupPresenter
		addItem: [ :item |
			aBlock value: item.
			item ]
]
