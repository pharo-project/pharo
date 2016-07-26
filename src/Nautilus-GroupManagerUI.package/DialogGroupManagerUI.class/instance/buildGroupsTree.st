tree
buildGroupsTree

	groupTree := (MorphTreeMorph on: self groupTreeModel)
		beSingle;
		autoDeselection: true;
		rowInset: 4;
		columnInset: 4;
		getMenuSelector: #menu:shifted:;
		nodeStringGetter: #name;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
		
	^groupTree buildContents.