Extension { #name : #SpMenuItemPresenter }

{ #category : #'*Spec2-Examples' }
SpMenuItemPresenter class >> exampleShowAtPointer [

	^ SpMenuPresenter new
		addItem: [ :item | item name: 'Test 1'; action: [ "Action" ] ];
		addItem: [ :item | item name: 'Test 2'; action: [ "action" ] ];
		addItem: [ :item | item name: 'Test 3'; action: [ "Action" ] ];
		openWithSpecAt: self currentHand position.
]

{ #category : #'*Spec2-Examples' }
SpMenuItemPresenter class >> exampleWithSubMenu [

	^ SpMenuPresenter new
		addItem: [ :item | item name: 'Test 1'; action: [ "action" ] ];
		addItem: [ :item | 
			item 
				name: 'Test 2'; 
				subMenu: (SpMenuPresenter new 
					addItem: [ :subMenuItem | subMenuItem name: 'SubMenu 1'; action: [ ] ];
					addItem: [ :subMenuItem | subMenuItem name: 'SubMenu 2'; action: [ ] ];
					addItem: [ :subMenuItem | subMenuItem name: 'SubMenu 3'; action: [ ] ];
					yourself) ];
		addItem: [ :item | item name: 'Test 3'; action: [ "Action" ] ];
		openWithSpecAt: self currentHand position.
]

{ #category : #'*Spec2-Examples' }
SpMenuItemPresenter class >> exampleWithinMenuButton [
	| presenter menuButton loremIpsumWords |

	(presenter := SpPresenter new) 
		layout: (SpBoxLayout newTopToBottom
			add: (SpBoxLayout newLeftToRight
					addLast: (menuButton := presenter newMenuButton) expand: false;
					yourself)
				expand: false;
			yourself).	

	loremIpsumWords := String loremIpsum substrings.
	menuButton menu: [ 
		SpMenuPresenter new
			addGroup: [ :group | group
				addItem: [ :item | item name: '1: ', loremIpsumWords atRandom ];
				addItem: [ :item | item name: '2: ', loremIpsumWords atRandom ];
				addItem: [ :item | item name: '3: ', loremIpsumWords atRandom ] ];
			yourself ].
		
	^ presenter openWithSpec
]
