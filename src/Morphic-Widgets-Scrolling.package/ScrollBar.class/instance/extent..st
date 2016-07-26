geometry
extent: p 
	p x > p y
		ifTrue: [super
				extent: (p max: 42 @ 8)]
		ifFalse: [super
				extent: (p max: 8 @ 42)].
	