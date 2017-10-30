instance creation
owner: anOwningPresenter

	^ self basicNew
		owner: anOwningPresenter;
		initialize;
		yourself