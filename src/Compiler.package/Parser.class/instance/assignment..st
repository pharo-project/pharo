expression types
assignment: varNode
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true