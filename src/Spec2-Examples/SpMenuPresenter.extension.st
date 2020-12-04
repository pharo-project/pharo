Extension { #name : #SpMenuPresenter }

{ #category : #'*Spec2-Examples' }
SpMenuPresenter class >> exampleShowAtPointer [

	^ self new 
		addItem: [ :item | item name: 'Test 1'; action: [ "Action" ] ];
		addItem: [ :item | item name: 'Test 2'; action: [ "Action" ] ];
		addItem: [ :item | item name: 'Test 3'; action: [ "Action" ] ];
		openWithSpecAt: self currentHand position
]

{ #category : #'*Spec2-Examples' }
SpMenuPresenter class >> exampleWithGroups [

	^ self new 
		addGroup: [ :group | group  
			addItem: [ :item | item name: 'Group 1. Test 1'; action: [ "Action" ] ];
			addItem: [ :item | item name: 'Group 1. Test 2'; action: [ "Action" ] ];
			addItem: [ :item | item name: 'Group 1. Test 3'; action: [ "Action" ] ] ];
		addGroup: [ :group | group  
			addItem: [ :item | item name: 'Group 2. Test 1'; action: [ "Action" ] ];
			addItem: [ :item | item name: 'Group 2. Test 2'; action: [ "Action" ] ];
			addItem: [ :item | item name: 'Group 2. Test 3'; action: [ "Action" ] ] ];
		openWithSpecAt: self currentHand position	
]
