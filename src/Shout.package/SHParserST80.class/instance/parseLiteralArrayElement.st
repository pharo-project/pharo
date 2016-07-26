parse
parseLiteralArrayElement
	| type |
	(currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		ifTrue: [type := (#('true' 'false' 'nil' ) includes: currentToken)
						ifTrue: [currentToken asSymbol]
						ifFalse: [#symbol].
			^ self scanPast: type].
	currentTokenFirst == $(
		ifTrue: [self scanPast: #arrayStart.
			^ self parseArray].
	currentTokenFirst == $[
		ifTrue: [self scanPast: #byteArrayStart.
			^ self parseByteArray].
	^ self parseLiteral: true