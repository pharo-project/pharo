instruction decoding
doPop

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == CaseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].