parse
parseLiteral: inArray 
	| pos c |
	currentTokenFirst == $$
		ifTrue: [self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			^ self
				scanPast: #character
				start: pos
				end: pos].
	currentTokenFirst isDigit
		ifTrue: [^ self scanPast: #number].
	currentToken = '-'
		ifTrue: [c := self currentChar.
			(inArray
					and: [c isNil
							or: [c isDigit not]])
				ifTrue: [^ self scanPast: #symbol].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			^ self scanPast: #number].
	currentTokenFirst == $'
		ifTrue: [^ self parseString].
	currentTokenFirst == $#
		ifTrue: [^ self parseSymbol].
	(inArray
			and: [currentToken notNil])
		ifTrue: [^ self scanPast: #symbol].
	self failWhen: currentTokenFirst == $..
	self error