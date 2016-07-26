tests
testDayOfWeek

	| dayNames sundayIndex |
	
	sundayIndex := self dateClass dayOfWeek: #Sunday.
	self assert: sundayIndex equals: 1.
	
	dayNames := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	dayNames doWithIndex: [ :e :i | | dayIndex |
		dayIndex := self dateClass dayOfWeek: e.
		self assert: dayIndex equals: i ].