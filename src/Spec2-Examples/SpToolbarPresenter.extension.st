Extension { #name : #SpToolbarPresenter }

{ #category : #'*Spec2-Examples' }
SpToolbarPresenter class >> example [
	| loremIpsumWords menuButton |
	
	loremIpsumWords := String loremIpsum substrings.
	^ self new 
		beBoth;
		add: (SpToolbarButtonPresenter new 
			icon: (self iconNamed: #smallOk);
			label: 'Simple';
			action: [ UIManager default defer: [ self inform: 'Pressed' ] ];
			yourself);
		add: (SpToolbarToggleButtonPresenter new
			icon: (self iconNamed: #smallOk);
			label: 'Toggle';
			action: [ :value | UIManager default defer: [ self inform: value asString ] ];
			yourself);
		add: ((menuButton := SpToolbarMenuButtonPresenter new)
			label: 'Menu';
			icon: (self iconNamed: #smallOk);
			action: [ UIManager default defer: [ self inform: menuButton label ] ];
			menu: [ 
				SpMenuPresenter new
					addGroup: [ :group | 
						1 to: 10 do: [ :index |
							group addItem: [ :item | item 
								name: ('{1}: {2}' format: { index. loremIpsumWords atRandom });
								action: [ menuButton label: item name ] ] ] ];
					yourself ];
			yourself);
		openWithSpec
]
