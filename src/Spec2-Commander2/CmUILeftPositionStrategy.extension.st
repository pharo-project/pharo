Extension { #name : #CmUILeftPositionStrategy }

{ #category : #'*Spec2-Commander2' }
CmUILeftPositionStrategy >> addButton: aButtonPresenter toActionBar: anActionBarPresenter [
	anActionBarPresenter add: aButtonPresenter
]

{ #category : #'*Spec2-Commander2' }
CmUILeftPositionStrategy >> addButton: aButtonPresenter toToolbar: aToolbarPresenter [
	aToolbarPresenter addItemLeft: aButtonPresenter
]
