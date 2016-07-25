private
prefixTester

	| p prefixChar |
	prefixes isEmpty ifTrue: [^nil].
	p := self optimizeSet: prefixes. "also allows copying closures"
	ignoreCase ifTrue: [p := p collect: [:each | each asUppercase]].
	^p size = 1 "might be a pretty common case"
		ifTrue: 
			[prefixChar := p first.
			ignoreCase
				ifTrue: [[:char :matcher | char sameAs: prefixChar]]
				ifFalse: [[:char :matcher | char = prefixChar]]]
		ifFalse:
			[ignoreCase
				ifTrue: [[:char :matcher | p includes: char asUppercase]]
				ifFalse: [[:char :matcher | p includes: char]]]