"
I implement the fact that the button corresponding to a command is disabled when it can not be run.
"
Class {
	#name : #SpDisableWhenCantBeRun,
	#superclass : #SpCommandDisplayStrategy,
	#category : #'Spec2-Commander2-Strategies'
}

{ #category : #displaying }
SpDisableWhenCantBeRun >> display: aCmSpecCommand in: aMenuOrGroupPresenter do: aBlock [
	aMenuOrGroupPresenter
		addItem: [ :item |
			aBlock value: item.
			item enabled: aCmSpecCommand canBeExecuted.
			item ]
]
