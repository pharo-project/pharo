visiting
visitArgumentNodes: aNodeCollection 
	^aNodeCollection collect: [:each | self visitArgumentNode: each]