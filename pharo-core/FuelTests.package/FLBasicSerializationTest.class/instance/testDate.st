tests
testDate

	self assertSerializationEqualityOf: (Date fromDays: 37023).
	self assertSerializationEqualityOf: (Date today).
	self assertSerializationEqualityOf: (Date year: 3050 month: 12 day: 31).
	self assertSerializationEqualityOf: (Date year: 1600 month: 12 day: 31).
	 
	
	
	
	