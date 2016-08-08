tests parsing
testParseUnfinishedBlockWithFaultyBody
	| node unfinishedBlockExp missingArgExp unfinishedBlockAndMissingArgExp validBlock |
	"Parsing a block as a faulty expression, where the block body is faulty and there is a missing closing bracket,
the whole expression is replaced by a single RBParseErrorNode, this makes it difficult for the highlighter to 
indicate the invalid expression."
	unfinishedBlockExp := '[:x | x missingArg: 3. '.
	missingArgExp := '[:x | x missingArg: . ]'.
	unfinishedBlockAndMissingArgExp := '[:x | x missingArg: . '.
	validBlock := '[:x | x missingArg: 3 ]. '.
	
	{unfinishedBlockExp. missingArgExp. unfinishedBlockAndMissingArgExp}
		do: [ :faultyExpression | 
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isBlock.
			self assert: node isFaulty.
			self assert: node body isFaulty ].
		
	node := self parseFaultyExpression: validBlock.
	self assert: node isBlock.
	self deny: node isFaulty.
	self deny: node body isFaulty