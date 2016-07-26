tests - variables
testSetLinkOnInstanceVariable
	| ivar link |
	selector := #exampleIvarRead.
	link := MetaLink new.
	ivar := ReflectivityExamples slotNamed: #ivar.
	ivar link: link.
	self assert: (ivar links includes: link).
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	link uninstall.