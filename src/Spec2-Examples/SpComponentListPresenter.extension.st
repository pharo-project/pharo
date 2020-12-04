Extension { #name : #SpComponentListPresenter }

{ #category : #'*Spec2-Examples' }
SpComponentListPresenter class >> example [
	| list |
	
	list := { 
		SpLabelPresenter new label: 'Test 1'; yourself.
		SpImagePresenter new image: (self iconNamed: #smallOk); yourself.
		SpButtonPresenter new label: 'A button'; yourself }.

	^ self new 
		presenters: list;
		openWithSpec
]
