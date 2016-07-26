private composition
completeComposition
	numberOfPhysicalLines := 1.
	actualWidth := 1.
	self lines
		do: [ :line | 
			line lineNumber: numberOfPhysicalLines.
			line stopCondition = #cr
				ifTrue: [ numberOfPhysicalLines := numberOfPhysicalLines + 1 ].
			actualWidth := line actualWidth max: actualWidth ]