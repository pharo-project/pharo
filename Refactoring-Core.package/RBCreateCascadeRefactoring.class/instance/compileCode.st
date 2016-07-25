transforming
compileCode
	class compileTree: (RBParseTreeRewriter
		replaceStatements: sequenceNode formattedCode
		with: transformedNode formattedCode
		in: self parseTree
		onInterval: selectedInterval)