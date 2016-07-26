tests - variables
testSetLinkOnGlobalVariable
	| globalVar link |
	selector := #exampleGlobalRead.
	link := MetaLink new.
	globalVar := (Smalltalk globals at: #GlobalForTesting) binding.
	globalVar link: link. 
	self assert: (globalVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	link uninstall.
	globalVar removeProperty: #links.
	