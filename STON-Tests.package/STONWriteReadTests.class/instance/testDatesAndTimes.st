tests
testDatesAndTimes
	| datesAndTimes |
	datesAndTimes := STON listClass withAll: { 
		Time fromSeconds: 
		Time now asSeconds. 
		Date today. 
		DateAndTime now }.
	self serializeAndMaterialize: datesAndTimes
