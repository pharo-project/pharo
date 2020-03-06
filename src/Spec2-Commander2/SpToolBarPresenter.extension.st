Extension { #name : #SpToolBarPresenter }

{ #category : #'*Spec2-Commander2' }
SpToolBarPresenter >> fillWith: aCommandGroup [

	self removeAllItems.
	SpToolBarPresenterBuilder new 
		toolbarPresenter: self;
		visit: aCommandGroup
]
