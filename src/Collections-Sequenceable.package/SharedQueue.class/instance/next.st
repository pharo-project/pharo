accessing
next
	^monitor critical: [
		monitor waitWhile: [ items isEmpty ].
		items removeFirst ]
