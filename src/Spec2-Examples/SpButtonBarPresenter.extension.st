Extension { #name : #SpButtonBarPresenter }

{ #category : #'*Spec2-Examples' }
SpButtonBarPresenter class >> example [
	| presenter list buttonBar |

	"A generic presenter with a list and an action bar"	
	(presenter := SpPresenter new) 
		layout: (SpBoxLayout newVertical
			spacing: 5; 
			add: (list := presenter newList);
			add: (buttonBar := presenter newButtonBar) expand: false;
			yourself).	
		
	"some settings to the list (to not have the presenter empty)"
	list 
		items: Smalltalk allClassesAndTraits;
		icons: [ :aClass | aClass systemIcon ];
		display: [ :aClass | aClass name ].
		
	"now, we add some buttons to our button bar"
	buttonBar
		"We want to place them at the end (as is often used in dialogs)"
		placeAtEnd;
		add: (presenter newButton
			icon: (presenter application iconNamed: #smallAdd);
			label: 'Add';
			yourself);
		add: (presenter newButton 
			icon: (presenter application iconNamed: #smallDelete);
			label: 'Delete';
			yourself);
		add: (presenter newButton
			icon: (presenter application iconNamed: #smallCopy);
			label: 'Copy';
			yourself).
			
	presenter openWithSpec
]
