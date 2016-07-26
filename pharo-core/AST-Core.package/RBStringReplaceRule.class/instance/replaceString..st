initialize-release
replaceString: replaceString 
	replaceTree := RBParser parseRewriteExpression: replaceString.
	searchTree isSequence = replaceTree isSequence 
		ifFalse: 
			[searchTree isSequence 
				ifTrue: [replaceTree := RBSequenceNode statements: (Array with: replaceTree)]
				ifFalse: [searchTree := RBSequenceNode statements: (Array with: searchTree)]]