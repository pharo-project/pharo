Extension { #name : #SpCheckBoxPresenter }

{ #category : #'*Spec2-Examples' }
SpCheckBoxPresenter class >> example [ 

	self new 
		label: 'Example';
		state: false;
		whenActivatedDo: [ 'Activated' crTrace ];
		whenDeactivatedDo: [ 'Deactivated' crTrace ];
		openWithSpec
]
