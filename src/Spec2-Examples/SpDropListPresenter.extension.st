Extension { #name : #SpDropListPresenter }

{ #category : #'*Spec2-Examples' }
SpDropListPresenter class >> example [

	^ (self new 
		help: 'Select the presenter.';
		items: SpPresenter withAllSubclasses;
		display: [ :aClass | aClass name ];
		whenSelectedItemChangedDo: [ :aClass | aClass crTrace ];
		openWithSpec)
		extent: 400@25;
		yourself
]
