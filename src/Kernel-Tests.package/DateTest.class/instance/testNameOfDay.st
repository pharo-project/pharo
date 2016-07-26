tests
testNameOfDay

	| dayNames firstDayName |
	
	firstDayName := self dateClass nameOfDay: 1.
	self assert: firstDayName equals: #Sunday.
	
	dayNames := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	(1 to: 7) do: [ :i | | dayName |
		dayName := self dateClass nameOfDay: i.
		self assert: dayName equals: (dayNames at: i) ].