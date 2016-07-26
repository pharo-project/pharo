tests
testDefaultEnvironment
	"self debug: #testDefaultEnvironment"
	
	"When no environment is specified, point to the default one"
	
	self assert: (SystemOrganizer new  environment == Smalltalk globals)