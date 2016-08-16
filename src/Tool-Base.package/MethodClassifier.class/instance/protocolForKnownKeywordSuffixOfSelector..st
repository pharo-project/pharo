private
protocolForKnownKeywordSuffixOfSelector: aSelector
	"Returns the protocol for the first keyword suffix that equals a keyword
	from aSelector or that a keyword ends with or nil if there isn't one."

	aSelector keywords
		do: [ :each | 
			self class keywordSuffixMapping
				keysAndValuesDo: [ :suffix :protocol | 
					(each = suffix or: [ each endsWith: suffix ])
						ifTrue: [ ^ protocol ] ] ].
	^ nil