tests - variables
testReifyGlobalVariableValue
	| globalVar instance |
	globalVar := GlobalForTesting binding.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	globalVar link: link.
	self assert: globalVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleGlobalRead = GlobalForTesting).
	self assert: (tag = #GlobalForTesting).