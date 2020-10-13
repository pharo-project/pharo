Extension { #name : #SpToggleButtonPresenter }

{ #category : #'*Spec2-Examples' }
SpToggleButtonPresenter class >> example [

	self new
		icon: (self iconNamed: #smallOk);
		whenActivatedDo: [ UIManager default defer: [ self inform: 'Activated' ] ];
		whenDeactivatedDo: [ UIManager default defer: [ self inform: 'Deactivated' ] ];
		openWithSpec
]
