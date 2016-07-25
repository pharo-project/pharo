parsing
grabBasicAddress
	"grad an address of the form a.b@c.d.e"
	self startNewAddress.
	"grab either the domain if specified, or the domain if not"
	self addToAddress.
	[tokens isEmpty not and: [ tokens last type = $.] ] 
		whileTrue: 
			["add name-dot pairs of tokens"
			self addToAddress.
			(#(Atom QuotedString ) includes: tokens last type)
				ifFalse: [self error: 'bad token in address: ' , tokens last text].
			self addToAddress].
	(tokens isEmpty or: [tokens last type ~= $@])
		ifTrue: ["no domain specified"
			self finishAddress]
		ifFalse: 
			["that was the domain.  check that no QuotedString's slipped in"
			curAddrTokens do: [:tok | tok type = #QuotedString ifTrue: [self error: 'quote marks are not allowed within a domain name (' , tok text , ')']].
			"add the @ sign"
			self addToAddress.
			"add the local part"
			(#(Atom QuotedString ) includes: tokens last type)
				ifFalse: [self error: 'invalid local part for address: ' , tokens last text].
			self addToAddress.
			"add word-dot pairs if there are any"
			[tokens isEmpty not and: [tokens last type = $.]]
				whileTrue: 
					[self addToAddress.
					(tokens isEmpty not and: [#(Atom QuotedString ) includes: tokens last type])
						ifTrue: [self addToAddress]].
			self finishAddress]