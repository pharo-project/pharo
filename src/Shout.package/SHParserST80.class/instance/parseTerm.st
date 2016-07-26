parse
parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst == $(
		ifTrue: [bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			^ bracketDepth := bracketDepth - 1].
	currentTokenFirst == $[
		ifTrue: [^ self parseBlock].
	currentTokenFirst == ${
		ifTrue: [self scanPast: #leftBrace.
			^ self parseBraceArray].
	self isName
		ifTrue: [^ self
				scanPast: (self resolve: currentToken)].
	self parseLiteral: false