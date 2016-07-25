building
buildPluginsTree
	| tree | 
	tree := IdentityMorphTreeMorph on: self.
	tree
		listManager: (IdentityMorphTreeListManager new client: tree);
		columns: {
				MorphTreeColumn new startWidth: 100;
					rowMorphGetSelector: #firstMorph;
					headerButtonLabel: 'Plugin name' font:  nil.
				MorphTreeColumn new startWidth: 75;
					rowMorphGetSelector: #secondMorph;
					headerButtonLabel: 'Position' font:  nil};
		withHLines: true;
		beMultiple;
		autoDeselection: false;
		rowInset: 4;
		columnInset: 4;
		getMenuSelector: #menu:shifted:.

	^ tree