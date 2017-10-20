running
setUp
	announcer := Announcer new.
	entity := OrderedCollection with: 1 with: 2 with: 3.
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a 
"			title: [:x | x printString ];"
			verticallyStackedArrangement.
		a list 
			title: [:x | x printString ];
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a tree 
			title: [:x | x printString ];
			children: #();
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a table
			title: [:x | x printString ];
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a text 
			updateOn: GLMTestAnnouncement from: [ announcer ].
		a list 
			title: 'Not updated automatically';
			shouldValidate: true.
		a label 
			display: [:x | x isEmpty ifTrue: ['#()'] ifFalse: [x last]];
			updateOn: GLMTestAnnouncement from: [ announcer ].			
			 ].
	browser transmit to: #two; from: #one; andShow: [ :a | a text].
	window := browser openOn: entity.
	listLabelMorph := 			window submorphs last submorphs second submorphs first submorphs 
									first tabSelectorMorph submorphs first submorphs first.
	listMorph := 				window submorphs last submorphs second submorphs first submorphs 
									first submorphs first submorphs first submorphs first.
	treeMorph := 				window submorphs last submorphs second submorphs first submorphs 
									second submorphs first submorphs first submorphs first.
	tableMorph := 				window submorphs last submorphs second submorphs first submorphs 
									third submorphs first submorphs first submorphs first.
	unupdatedListMorph := 	window submorphs last submorphs second submorphs first submorphs 
									fifth submorphs first submorphs first submorphs first.
	labelMorph := 				window submorphs last submorphs second submorphs first submorphs 
									last submorphs first submorphs first.