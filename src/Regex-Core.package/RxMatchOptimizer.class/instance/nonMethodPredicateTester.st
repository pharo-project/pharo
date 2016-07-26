accessing
nonMethodPredicateTester
	| p selector |
	nonMethodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonMethodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[(char perform: selector) not]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | (char perform: sel) not]]]]