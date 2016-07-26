halos and balloon help
hasHalo: aBool 
	super hasHalo: aBool.
	aBool
		ifFalse: [ (self hasProperty: #deleting) ifFalse: [self delete] ]
