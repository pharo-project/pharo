tokenizing
nextDomainLiteral
	| start end |
	start := pos.
	end := text indexOf: $] startingAt: start ifAbsent: [ 0 ].
	end = 0 ifTrue: [
		"not specified"
		self error: 'saw [ without a matching ]' ].

	pos := end+1.

	^MailAddressToken
		type: #DomainLiteral
		text: (text copyFrom: start to: end)