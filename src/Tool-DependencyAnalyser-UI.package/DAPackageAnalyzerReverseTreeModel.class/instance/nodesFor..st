private
nodesFor: anItemList
	^ anItemList collect: 
		[ :each | 
		DAReversePackageNode new
						content: each;
						treeModelParent: self;
						packageTreeNautilus: self packageTreeNautilus;
						packageTreeNautilusUI: self packageTreeNautilusUI;
						cycle: false;
						yourself ]