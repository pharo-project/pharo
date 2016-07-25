tests
testReshapeClassWithClassSlot
	
	aClass := 
		Object 
			subclass: self aClassName 
			instanceVariableNames: '' 
			classVariableNames: '' 
			poolDictionaries: ''
			category: self aCategory.	
	aClass class 
		instanceVariableNames: #x.
	
	self assert: aClass class instVarNames equals: #(x).
	
	aClass := 
		Object 
			subclass: self aClassName 
			instanceVariableNames: 'a' 
			classVariableNames: '' 
			poolDictionaries: ''
			category: self aCategory.	

	self assert: aClass class instVarNames equals: #(x).