initialization
initializeWidgets
	remotes := self newTree.
	addRemote := self newButton.
	removeRemote := self newButton.
	editRemote := self newButton.
	remotes
		beCheckList;
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		displayBlock: [ :item | item label ];
		iconBlock: [ :item | item icon ];
		roots: self buildRoots.
	addRemote
		icon: (Smalltalk ui icons iconNamed: #remotesManagerAddRemote);
		action: [ self addRemote ];
		help: 'Add a new remote'.
	removeRemote
		icon: (Smalltalk ui icons iconNamed: #remotesManagerRemoveRemote);
		action: [ self removeRemote ];
		enabled: false;
		help: 'Remote the selected remote'.
	editRemote
		icon: (Smalltalk ui icons iconNamed: #remotesManagerEditRemote);
		action: [ self editRemote ];
		enabled: false;
		help: 'Edit the selected remote'.
	self focusOrder
		add: remotes;
		add: removeRemote;
		add: addRemote