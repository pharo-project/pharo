accessing
tree
	^ tree ifNil: [ 
		tree := self instantiate: TreePresenter.
		tree
			isCheckList: true;
			multiSelection: true;
			autoMultiSelection: true;
		"	rootNodeHolder: [ :item | 
				TreeNodePresenter with: item model: tree ];"
	
			displayBlock:  [ :aPackageOrClass |
				self labelFor: aPackageOrClass ];
			childrenBlock: [ :aPackageOrClass | 
				self childrenFor: aPackageOrClass ];
			iconBlock: [ :treeNode | 
				self iconFor: treeNode content ];
			
			whenSelectedItemChanged: [ :selection | self selectionChanged ];
			doubleClick: [
				 Nautilus openOnPackage: tree selectedItem ]]