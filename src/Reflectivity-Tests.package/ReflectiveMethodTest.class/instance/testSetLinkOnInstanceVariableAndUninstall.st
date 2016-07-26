tests - variables
testSetLinkOnInstanceVariableAndUninstall
	| ivar link |
	selector := #exampleIvarRead.
	link := MetaLink new.
	ivar := ReflectivityExamples slotNamed: #ivar.
	ivar link: link.
	self assert: (ivar links includes: link).
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleIvarRead) linkCount = 1.
	link uninstall.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = CompiledMethod.
	self assert: (ivar links isNil).