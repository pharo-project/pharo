tests - variables
testReifyClassVariableEntity
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(entity).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag = classVar).