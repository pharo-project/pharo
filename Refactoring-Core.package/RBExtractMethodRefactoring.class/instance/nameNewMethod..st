transforming
nameNewMethod: aSymbol
	| args newSend |
	args := parameters collect: [ :parm | RBVariableNode named: parm ].
	extractedParseTree renameSelector: aSymbol andArguments: args asArray.
	aSymbol numArgs = 0
		ifTrue: [ modifiedParseTree := RBParseTreeRewriter replace: self methodDelimiter with: 'self ' , aSymbol asString in: modifiedParseTree.
			^ self ].
	newSend := String
		streamContents: [ :str | 
			str nextPutAll: 'self '.
			aSymbol keywords
				with: parameters
				do: [ :key :arg | 
					str
						nextPutAll: key asString;
						nextPut: $ ;
						nextPutAll: arg asString;
						nextPut: $  ] ].
	modifiedParseTree := RBParseTreeRewriter replace: self methodDelimiter with: newSend in: modifiedParseTree