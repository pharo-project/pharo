tests - variables
testReifySlotClass
	| iVar instance |
	iVar := ReflectivityExamples slotNamed: #ivar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(class).
	iVar link: link.
	self assert: iVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = ReflectivityExamples).