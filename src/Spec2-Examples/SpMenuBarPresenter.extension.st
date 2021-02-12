Extension { #name : #SpMenuBarPresenter }

{ #category : #'*Spec2-Examples' }
SpMenuBarPresenter class >> example [
	| presenter menuBar submenuFile submenuEdit |

	(presenter := SpPresenter new) 
		layout: (SpBoxLayout newTopToBottom 
			add: (menuBar := presenter newMenuBar) expand: false;
			add: (presenter newLabel label: 'Empty space');
			yourself).
			
	submenuFile := SpMenuPresenter new 
		addItem: [ :item | item name: 'Quit'; icon: (self iconNamed: #smallQuit); action: [ "Action" ] ];
		yourself.
			
	submenuEdit := SpMenuPresenter new 
		addItem: [ :item | item name: 'Copy'; icon: (self iconNamed: #smallCopy); action: [ "Action" ] ];
		addItem: [ :item | item name: 'Cut'; icon: (self iconNamed: #smallCut); action: [ "Action" ] ];
		addItem: [ :item | item name: 'Paste'; icon: (self iconNamed: #smallPaste); action: [ "Action" ] ];
		yourself.

	menuBar addGroup: [ :group | group
		addItem: [ :item | item name: 'File'; subMenu: submenuFile ];
		addItem: [ :item | item name: 'Edit'; subMenu: submenuEdit ] ].

	^ presenter openWithSpec
]
