ancestry
topologicalAncestors
	
	^ Array streamContents:
		[:s | | f frontier |
		frontier := MCFrontier frontierOn: self.
		[f := frontier frontier.
		s nextPutAll: f.
		frontier removeAll: f.
		f isEmpty] whileFalse] 