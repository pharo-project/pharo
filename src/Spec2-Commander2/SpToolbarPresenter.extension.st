Extension { #name : #SpToolbarPresenter }

{ #category : #'*Spec2-Commander2' }
SpToolbarPresenter >> fillWith: aCommandGroup [

	self removeAllItems.
	SpToolbarPresenterBuilder new 
		toolbarPresenter: self;
		visit: aCommandGroup
]
