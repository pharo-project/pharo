tests
testDateOfToDo
	
	| manifestBuilder date1 date2 |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	date1 :=  DateAndTime current.
	manifestBuilder addToDo: MFClassA of: 'test' version: 0. 
	date2 :=  DateAndTime current.
	
	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) >= date1 .
	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) <= date2 .
	
	
	
	