Class {
	#name : #ReflectivityReificationTest,
	#superclass : #TestCase,
	#instVars : [
		'tag',
		'link'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #tagging }
ReflectivityReificationTest >> tagExec [
	tag := #yes
]

{ #category : #tagging }
ReflectivityReificationTest >> tagExec: aTag [
	tag := aTag.
]

{ #category : #running }
ReflectivityReificationTest >> tearDown [
	link ifNotNil: [ link uninstall ].
	super tearDown 
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testAccessTemp [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements second.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: {RBVariableNode named: #temp}.
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableClass [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(class).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag == ReflectivityExamples).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableContext [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(context).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag class == Context).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableEntity [
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
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableLink [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(link).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag == link).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableName [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag = #ClassVar).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableObject [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(object).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag class == ReflectivityExamples).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableValue [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag = #AClassVar).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyClassVariableVariable [
	| classVar instance |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(variable).
	classVar link: link.
	self assert: classVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleClassVarRead = #AClassVar).
	self assert: (tag = classVar).
]

{ #category : #'tests - allnodes' }
ReflectivityReificationTest >> testReifyEntity [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleIvarRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(entity).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = varNode).

]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyGlobalName [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleGlobalRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleGlobalRead = GlobalForTesting).
	self assert: (tag = #GlobalForTesting).
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyGlobalValue [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleGlobalRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleGlobalRead = GlobalForTesting).
	self assert: (tag = GlobalForTesting).
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyGlobalVariable [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleGlobalRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(variable).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleGlobalRead = GlobalForTesting).
	self assert: (tag class == GlobalVariable).

]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifyGlobalVariableValue [
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
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyIvarName [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleIvarRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = 'ivar').
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyIvarValue [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleIvarRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = 33).
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyIvarVariable [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleIvarRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(variable).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag isKindOf: Slot).
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodArgs [
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleWithArg:) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: tag equals: #(3)
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodArgsAfter [
	| methodNode instance |
	methodNode := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		control: #after;
		arguments: #(arguments).
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleWithArg:) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: tag equals: #(3)
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodArgsByNameBefore [
	| methodNode instance |
	methodNode := (ReflectivityExamples >> #exampleWithArg:) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		control: #before;
		arguments: methodNode arguments.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleWithArg:) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: (instance exampleWithArg: 3) equals: 5.
	self assert: tag equals: 3
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodReceiver [
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(receiver).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleMethod) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethod equals: 5.
	self assert: tag equals: instance
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodSelector [
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(selector).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleMethod) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethod equals: 5.
	self assert: tag equals: #exampleMethod
]

{ #category : #'tests - method' }
ReflectivityReificationTest >> testReifyMethodThisContext [
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(context).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self
		assert: (ReflectivityExamples >> #exampleMethod) class
		equals: ReflectiveMethod.
	self assert: tag isNil.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethod equals: 5.
	self assert: tag class equals: Context
]

{ #category : #'tests - assignment' }
ReflectivityReificationTest >> testReifyNewValueAssignment [
	| varNode instance|
	varNode := (ReflectivityExamples>>#exampleAssignment) assignmentNodes first.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(newValue).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).

]

{ #category : #'tests - allnodes' }
ReflectivityReificationTest >> testReifyNode [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleIvarRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(node).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = varNode).


]

{ #category : #'tests - allnodes' }
ReflectivityReificationTest >> testReifyObject [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(object).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag = instance).
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 

]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifyReceiverOnSendWithArguments [
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
]

{ #category : #'tests - return' }
ReflectivityReificationTest >> testReifyReturnValue [
	| returnNode instance |
	returnNode := (ReflectivityExamples>>#exampleAssignment) ast body statements last.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	returnNode link: link.
	self assert: returnNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).
]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendAll [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: #receiver; 
		selector: #perform:withArguments:;
		arguments: #(selector arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).

]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendArgsAsArray [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(selector context);
		options: #(argsAsArray).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag isArray).
	self assert: (tag first == #+).
	self assert: (tag second class == Context).
	
]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendArguments [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag = #(3)).

]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendArguments2 [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleSendTwoArgs) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleSendTwoArgs) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleSendTwoArgs = #(1 2)).
	self assert: (tag = #(1 2)).

]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendReceiver [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(receiver).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag = 2).

]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendSelector [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(selector).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag = #+).
]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendThisContext [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(context).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag class = Context).
]

{ #category : #'tests - sends' }
ReflectivityReificationTest >> testReifySendValue [
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		control: #after;
		option: #(+ optionWeakAfter);
		arguments: #(value).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleMethod = 5).
	self assert: (tag = 5).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifySlotClass [
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
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifySlotName [
	| iVar instance |
	iVar := ReflectivityExamples slotNamed: #ivar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	iVar link: link.
	self assert: iVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = #ivar).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifySlotValue [
	| iVar instance |
	iVar := ReflectivityExamples slotNamed: #ivar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	iVar link: link.
	self assert: iVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = 33).
]

{ #category : #'tests - variables' }
ReflectivityReificationTest >> testReifySlotVariable [
	| iVar instance |
	iVar := ReflectivityExamples slotNamed: #ivar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(variable).
	iVar link: link.
	self assert: iVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = iVar).
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyTempName [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements first variable.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag ='temp').
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyTempNewValue [
	| varNode instance|
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements first variable.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(newValue).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).
]

{ #category : #'tests - variablenodes' }
ReflectivityReificationTest >> testReifyTempValue [
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).
]
