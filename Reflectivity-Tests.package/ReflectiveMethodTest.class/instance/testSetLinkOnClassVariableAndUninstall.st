tests - variables
testSetLinkOnClassVariableAndUninstall
	| classVar link |
	selector := #exampleClassVarRead.
	link := MetaLink new.
	classVar := (ReflectivityExamples classVariableNamed: #ClassVar).
	classVar link: link.

	self assert: (classVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) linkCount = 1.
	
	link uninstall.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = CompiledMethod.
