accessing
treeMatching: aString in: aParseTree 
	(self new)
		matches: aString do: [:aNode :answer | ^aNode];
		executeTree: aParseTree.
	^nil