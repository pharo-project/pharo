private
determineTestMethod
	"Answer a block closure that will work as a can-match predicate.
	Answer nil if no viable optimization is possible (too many chars would
	be able to start a match)."

	| testers |
	(conditions includes: #any) ifTrue: [^nil].
	testers := OrderedCollection new: 5.
	#(#prefixTester #nonPrefixTester #conditionTester #methodPredicateTester #nonMethodPredicateTester #predicateTester #nonPredicateTester)
		do: 
			[:selector | 
			| tester |
			tester := self perform: selector.
			tester notNil ifTrue: [testers add: tester]].
	testers isEmpty ifTrue: [^nil].
	testers size = 1 ifTrue: [^testers first].
	testers := testers asArray.
	^[:char :matcher | testers contains: [:t | t value: char value: matcher]]