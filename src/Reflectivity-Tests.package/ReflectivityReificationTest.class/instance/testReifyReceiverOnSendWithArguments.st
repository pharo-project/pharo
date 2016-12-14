tests - sends
testReifyReceiverOnSendWithArguments
	| sendNode instance beforeSize |
	sendNode := (ReflectivityExamples>>#exampleSendNoReturn) ast body statements last.
	beforeSize := 0.
	link := MetaLink new 
		metaObject: [:receiver | beforeSize := receiver size]; 
		selector: #value:;
		control: #before;
		arguments: #(receiver).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleSendNoReturn) class equals: ReflectiveMethod.
	self assert: beforeSize equals: 0.
	instance := ReflectivityExamples new .
	instance exampleSendNoReturn.
	self assert: beforeSize equals: 2.