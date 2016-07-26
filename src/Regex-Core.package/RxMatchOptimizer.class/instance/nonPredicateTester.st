private
nonPredicateTester

	| p pred |
	nonPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[pred := p first.
			[:char :matcher | (pred value: char) not]]
		ifFalse: 
			[[:char :m | p contains: [:some | (some value: char) not]]]