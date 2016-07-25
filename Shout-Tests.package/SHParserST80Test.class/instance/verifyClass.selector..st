utilities
verifyClass: aBehavior selector: aSymbol
	| ranges position types errors |
	ranges := SHParserST80 new 
		rangesIn: (aBehavior sourceCodeAt: aSymbol) 
		classOrMetaClass: aBehavior 
		workspace: nil environment: nil.
	types := #(#default #invalid #excessCode #comment #unfinishedComment #'$' #character #integer #number #- #symbol #stringSymbol #literalArray #string #unfinishedString #assignment #ansiAssignment #literal #keyword #binary #unary #incompleteKeyword #incompleteBinary #incompleteUnary #undefinedKeyword #undefinedBinary #undefinedUnary #patternKeyword #patternBinary #patternUnary #self #super #true #false #nil #thisContext #return #patternArg #methodArg #blockPatternArg #blockArg #argument #blockArgColon #leftParenthesis #rightParenthesis #leftParenthesis1 #rightParenthesis1 #leftParenthesis2 #rightParenthesis2 #leftParenthesis3 #rightParenthesis3 #leftParenthesis4 #rightParenthesis4 #leftParenthesis5 #rightParenthesis5 #leftParenthesis6 #rightParenthesis6 #leftParenthesis7 #rightParenthesis7 #blockStart #blockEnd #blockStart1 #blockEnd1 #blockStart2 #blockEnd2 #blockStart3 #blockEnd3 #blockStart4 #blockEnd4 #blockStart5 #blockEnd5 #blockStart6 #blockEnd6 #blockStart7 #blockEnd7 #arrayStart #arrayEnd #arrayStart1 #arrayEnd1 #byteArrayStart #byteArrayEnd #byteArrayStart1 #byteArrayEnd1 #leftBrace #rightBrace #cascadeSeparator #statementSeparator #externalCallType #externalCallTypePointerIndicator #primitiveOrExternalCallStart #primitiveOrExternalCallEnd #methodTempBar #blockTempBar #blockArgsBar #primitive #externalFunctionCallingConvention #module #blockTempVar #blockPatternTempVar #instVar #workspaceVar #undefinedIdentifier #incompleteIdentifier #tempVar #patternTempVar #poolConstant #classVar #globalVar #pragmaKeyword #pragmaUnary #pragmaBinary) asIdentitySet.
	errors := #(#excessCode #unfinishedComment #unfinishedString) asIdentitySet.
	position := 0.
	ranges do: [ :each |
		self 
			assert: 1 <= each length 
			description: 'empty range'.
		self 
			assert: position <= each start
			description: 'overlapping range'.
		self 
			assert: each start <= each end 
			description: 'empty range'.
		self 
			assert: (types includes: each type)
			description: 'unknown type ' , each type.
		self
			deny: (errors includes: each type)
			description: 'error type ' , each type.
		position := each end ]