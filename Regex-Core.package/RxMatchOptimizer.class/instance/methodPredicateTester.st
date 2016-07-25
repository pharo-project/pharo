accessing
methodPredicateTester
	| p selector |
	methodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: methodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			["might be a pretty common case"
			selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[char perform: selector]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | char perform: sel]]]]