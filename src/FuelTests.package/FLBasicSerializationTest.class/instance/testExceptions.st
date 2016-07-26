tests
testExceptions
	| ex |
	ex := (Error new messageText: 'sample error').
	self assert: (self resultOfSerializeAndMaterialize: ex) messageText equals: ex messageText.
	
	ex := (Warning new messageText: 'sample warning').
	self assert: (self resultOfSerializeAndMaterialize: ex) messageText equals: ex messageText.
	
	ex := (Notification new messageText: 'sample notification').
	self assert: (self resultOfSerializeAndMaterialize: ex) messageText equals: ex messageText.
	
