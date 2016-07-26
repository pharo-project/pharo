tests
testDateOfFalsePositive
	
	| manifestBuilder date1 date2 |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	date1 :=  DateAndTime current.
	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0. 
	date2 :=  DateAndTime current.
	
	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) >= date1 .
	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) <= date2 .
	
	
	
	