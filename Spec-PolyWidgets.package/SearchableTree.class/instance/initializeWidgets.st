initialization
initializeWidgets

	tree := self newTree.
	inputField := self newTextInput.
	label := self newLabel.
	button := self newButton.
			
	tree rootNodeHolder: [ :item | 
		(tree defaultNodeFor: item)
			matchTokenFilterBlock: [ :aFilter |
				aFilter token
					ifEmpty: [ true ]
					ifNotEmpty: [ item asString asLowercase includesSubstring: aFilter token asLowercase ] ];
			yourself ].
			
	button 
		label: '';
		icon: (Smalltalk ui icons iconNamed: #smallFindIcon);
		state: false;
		action: [ inputField accept ].
			
	label 
		label: 'Search:';
		color:Color black.
	
	inputField
		ghostText: 'Search';
		entryCompletion: nil;
		acceptOnCR: true.
		
	self focusOrder
		add: tree;
		add: inputField