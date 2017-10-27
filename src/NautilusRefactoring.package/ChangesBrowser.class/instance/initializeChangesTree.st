initialization
initializeChangesTree
	changesTree
		beCheckList;
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		rootNodeHolder: [ :item | 
					TreeNodePresenter new
						content: item;
						hasContentToShow: true;
						children: [ changesTree childrenFor: item ] ];
		displayBlock: [ :item | item nameToDisplay ].
	self whenBuiltDo: [ changesTree selectAll ]