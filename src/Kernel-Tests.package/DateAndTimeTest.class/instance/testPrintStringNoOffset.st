tests - under design
testPrintStringNoOffset
	
	| localOffsetHours localOffsetMinutes signString |
	signString := DateAndTime localOffset hours positive ifTrue: [ '+' ] ifFalse: [ '-' ].
	localOffsetHours := DateAndTime localOffset hours abs printStringPadded: 2.
	localOffsetMinutes := DateAndTime localOffset minutes printStringPadded: 2.
	self assert: ('2002-05-16T17:20' asDateAndTime printString
						 = ('2002-05-16T17:20:00{1}{2}:{3}' format: { signString . localOffsetHours. localOffsetMinutes})).
	self assert: ('2002-05-16T17:20:45' asDateAndTime printString
						= ('2002-05-16T17:20:45{1}{2}:{3}' format: { signString . localOffsetHours. localOffsetMinutes})).