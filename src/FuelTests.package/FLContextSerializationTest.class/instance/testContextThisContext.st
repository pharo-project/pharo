tests
testContextThisContext

	| materializedContext context |
	context := thisContextSample.
	materializedContext := self resultOfSerializeAndMaterialize: context.
	
	self deny:  context == materializedContext.
	"I cannot compare by = because MethodContext do not implement it and it will finally use the identity."
	self assert: context pc = materializedContext pc.
	self assert: context stackPtr = materializedContext stackPtr.
	self assert: context method = materializedContext method.
	self assert: context receiver = materializedContext receiver.
	"I cannot compare by = because MethodContext do not implement it and it will finally use the identity."
	self deny: context sender ==  materializedContext sender.
	self assert: context sender pc == materializedContext sender pc.
	self assert: context sender stackPtr = materializedContext sender stackPtr.
	"etc...."
