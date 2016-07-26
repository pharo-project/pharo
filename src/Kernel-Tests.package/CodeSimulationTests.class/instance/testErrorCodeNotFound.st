tests - primitives
testErrorCodeNotFound
	| ctx result resultSimu |
	
	self skip.
	
	Smalltalk vm isRunningCog ifFalse: [^self].
	
	result := self veryBasicAt: 1.
		
	ctx := Context 
		sender: nil
		receiver: nil
		method: (Object>>#at: )
		arguments: #(10).
	
	resultSimu := ctx 
		push: nil;
		push: 500;
		doPrimitive: 117  method:  (self class>>#veryBasicAt:) receiver: self args: #(999).
		
	self assert: resultSimu isArray.	
	self assert: Context  primitiveFailToken first == resultSimu first.
	self assert: result = resultSimu second.