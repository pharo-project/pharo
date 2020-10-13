Extension { #name : #SpMenuGroupPresenter }

{ #category : #'*Spec2-Examples' }
SpMenuGroupPresenter class >> exampleMenuGroup [
	| menu |
	
	menu := SpMenuPresenter new 
		addGroup: [ :aGroup |
			aGroup addItem: [ :anItem |
				anItem name: 'Item Group 1'; action: [ "action here" ] ] ];
		addGroup: [ :aGroup |
			aGroup addItem: [ :anItem |
				anItem name: 'Item Group 2'; action: [ "action here" ] ] ].
	
	SpMenuButtonPresenter new 
		menu: menu;
		openWithSpec
]
