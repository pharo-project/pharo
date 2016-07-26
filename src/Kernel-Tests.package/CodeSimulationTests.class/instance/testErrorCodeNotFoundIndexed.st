tests - primitives
testErrorCodeNotFoundIndexed
	| ctx result resultSimu |
	
	Smalltalk vm isRunningCog ifFalse: [^self].
	
	result := self indexedBasicAt: 100.
		
	ctx := Context 
		sender: nil
		receiver: nil
		method: (Object>>#at: )
		arguments: #(10).
	
	resultSimu := ctx 
		push: nil;
		push: 500;
		doPrimitive: 60  method:  (self class>>#indexedBasicAt:) receiver: self args: #(100).
		
	self assert: resultSimu isArray.	
	self assert: resultSimu size = 2.
	self assert: Context  primitiveFailToken first == resultSimu first.
	self assert: result = resultSimu second.