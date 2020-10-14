Extension { #name : #SpActionBarPresenter }

{ #category : #'*Spec2-Examples' }
SpActionBarPresenter class >> example [
	| presenter list actionBar |

	"A generic presenter with a list and an action bar"	
	(presenter := SpPresenter new) 
		layout: (SpBoxLayout newVertical 
			add: (list := presenter newList);
			add: (actionBar := presenter newActionBar) expand: false;
			yourself).	
		
	"some settings to the list (to not have the presenter empty)"
	list 
		items: Smalltalk allClassesAndTraits;
		icons: [ :aClass | aClass systemIcon ];
		display: [ :aClass | aClass name ].
		
	"now, we add some buttons to our action bar"
	actionBar 
		add: (presenter newButton
			icon: (presenter application iconNamed: #smallAdd);
			label: 'Add';
			yourself);
		addLast: (presenter newButton 
			icon: (presenter application iconNamed: #smallDelete);
			label: 'Delete';
			yourself);
		addLast: (presenter newButton
			icon: (presenter application iconNamed: #smallCopy);
			label: 'Copy';
			yourself).
			
	^ presenter openWithSpec
]
