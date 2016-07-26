accessing
buildRoots
	self tree
		roots: cycles;
		whenBuiltDo: [ 
					tree
						rootNodeHolder: [ :item | 
							DACycleNode new
								content: item;
								treeModelParent: self;
								packageTreeNautilus: self packageTreeNautilus;
								packageTreeNautilusUI: self packageTreeNautilusUI;
								cycle: true ] ].
	self cyclesLabel
		label:
			'Analysis of ' , self cycles size asString , ' cycles'.