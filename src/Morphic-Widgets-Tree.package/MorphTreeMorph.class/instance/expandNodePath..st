expanding-collapsing
expandNodePath: aNodePath
	(self allNodeMorphs at: 1 ifAbsent: [^self]) 
		openNodePath: aNodePath