Extension { #name : #CmUIRightPositionStrategy }

{ #category : #'*Spec2-Commander2' }
CmUIRightPositionStrategy >> addButton: aButtonPresenter toActionBar: anActionBarPresenter [
	anActionBarPresenter addLast: aButtonPresenter
]

{ #category : #'*Spec2-Commander2' }
CmUIRightPositionStrategy >> addButton: aButtonPresenter toToolbar: aToolbarPresenter [
	aToolbarPresenter addItemRight: aButtonPresenter
]
