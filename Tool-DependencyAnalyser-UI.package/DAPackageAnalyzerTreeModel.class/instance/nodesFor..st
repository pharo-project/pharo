private
nodesFor: anItemList 
	^ anItemList sorted collect: 
		[ :each | 
		DAPackageUnderAnalysisNode new
			content: each;
			cycle: false;
			treeModelParent: self;
			packageTreeNautilus: self packageTreeNautilus;
			packageTreeNautilusUI: self packageTreeNautilusUI;
			yourself ]