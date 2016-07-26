private
nonPrefixTester

	| np nonPrefixChar |
	nonPrefixes isEmpty ifTrue: [^nil].
	np := self optimizeSet: nonPrefixes. "also allows copying closures"
	^np size = 1 "might be be pretty common case"
		ifTrue: 
			[nonPrefixChar := np first.
			[:char :matcher | char ~= nonPrefixChar]]
		ifFalse: [[:char : matcher | (np includes: char) not]]