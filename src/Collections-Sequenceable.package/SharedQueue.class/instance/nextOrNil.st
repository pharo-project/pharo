accessing
nextOrNil
	^monitor critical: [
		items isEmpty ifTrue: [ nil ] ifFalse: [ items removeFirst ] ]