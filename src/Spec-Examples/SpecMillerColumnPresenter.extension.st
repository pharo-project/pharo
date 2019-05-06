Extension { #name : #SpecMillerColumnPresenter }

{ #category : #'*Spec-Examples' }
SpecMillerColumnPresenter class >> example [
	^ SpecMillerColumnPresenter new
		presenterBlock: [ :model | 
			ListPresenter new
				items: (1 to: model);
				yourself ];
		setRootModel: 10;
		openWithSpec
]
