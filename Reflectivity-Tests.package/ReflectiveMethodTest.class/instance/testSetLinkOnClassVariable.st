tests - variables
testSetLinkOnClassVariable
	| classVar link |
	selector := #exampleClassVarRead.
	link := MetaLink new.
	classVar := (ReflectivityExamples classVariableNamed: #ClassVar).
	classVar link: link.

	self assert: (classVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	classVar removeProperty: #links.