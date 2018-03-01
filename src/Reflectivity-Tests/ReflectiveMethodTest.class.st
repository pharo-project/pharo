Class {
	#name : #ReflectiveMethodTest,
	#superclass : #TestCase,
	#instVars : [
		'selector'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #running }
ReflectiveMethodTest >> tearDown [
	selector ifNotNil: [  
		ReflectivityExamples recompile: selector].
	super tearDown 
]

{ #category : #'tests - creation' }
ReflectiveMethodTest >> testCreateReflectiveMethod [
	| compiledMethod reflectiveMethod |
	compiledMethod := ReflectivityExamples>>#exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class = ReflectiveMethod.
]

{ #category : #'tests - creation' }
ReflectiveMethodTest >> testCreateReflectiveMethodAndInvalidate [
	| compiledMethod reflectiveMethod |
	selector := #exampleMethod.
	compiledMethod := ReflectivityExamples>>#exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class = ReflectiveMethod.
	compiledMethod reflectiveMethod: reflectiveMethod. 
	self assert: (compiledMethod reflectiveMethod = reflectiveMethod).
	
	compiledMethod invalidate.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.
	
	
]

{ #category : #'tests - creation' }
ReflectiveMethodTest >> testInvalidate [
	selector := #exampleMethod.
	(ReflectivityExamples>>#exampleMethod) createTwin. 
	(ReflectivityExamples>>#exampleMethod) invalidate. 
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.
	
	
]

{ #category : #'tests - creation' }
ReflectiveMethodTest >> testInvalidateNoJit [
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) createTwin. 
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass)  invalidate. 
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass)  class = CompiledMethod.
	self assert: ReflectivityExamples exampleMethodWithMetaLinkOptionsViaClass = 5.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod. 
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod: nil.
	ReflectivityExamples class recompile: #exampleMethodWithMetaLinkOptionsViaClass.
	
]

{ #category : #'tests - creation' }
ReflectiveMethodTest >> testInvalidateWithLinkCount [
	selector := #exampleMethod.
	(ReflectivityExamples>>#exampleMethod) createTwin. 
	(ReflectivityExamples>>#exampleMethod) invalidate. 
	(ReflectivityExamples>>#exampleMethod) linkCount: 1.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	self deny: (ReflectivityExamples>>#exampleMethod) reflectiveMethod isNil.
	
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testLinkCountTwoLinks [
	| sendNode link1 link2 |
	selector := #exampleMethod.
	
	link1 := MetaLink new. 
	link2 := MetaLink new. 
	
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	sendNode link: link1.
	self assert:  (ReflectivityExamples>>#exampleMethod) linkCount = 1.
	sendNode link: link2.
	self assert: (ReflectivityExamples>>#exampleMethod) linkCount = 2.
	sendNode removeLink: link1.
	self assert: (ReflectivityExamples>>#exampleMethod) linkCount = 1.
	sendNode removeLink: link2.	
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#exampleMethod) reflectiveMethod isNil.
	
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testSetLink [
	| sendNode link |
	selector := #exampleMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples >> #exampleMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode hasMetalink: link).
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	sendNode removeLink: link.
	(ReflectivityExamples >> #exampleMethod) destroyTwin.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	self assert: (ReflectivityExamples >> #exampleMethod) reflectiveMethod isNil
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testSetLinkInPrimitive [
	| sendNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples>>#examplePrimitiveMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples>>#examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) reflectiveMethod isNil.
	(ReflectivityExamples>>#examplePrimitiveMethod) recompile
]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnClassVariable [
	| classVar link |
	selector := #exampleClassVarRead.
	link := MetaLink new.
	classVar := (ReflectivityExamples classVariableNamed: #ClassVar).
	classVar link: link.

	self assert: (classVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	classVar removeProperty: #links.
]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnClassVariableAndUninstall [
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

]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnGlobalVariable [
	| globalVar link |
	selector := #exampleGlobalRead.
	link := MetaLink new.
	globalVar := (Smalltalk globals at: #GlobalForTesting) binding.
	globalVar link: link. 
	self assert: (globalVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	link uninstall.
	globalVar removeProperty: #links.
	
]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnGlobalVariableAndUninstall [
	| global link |
	selector := #exampleGlobalRead.
	link := MetaLink new.
	global := (Smalltalk globals at: #GlobalForTesting) binding.
	global link: link.
	self assert: (global links includes: link). 
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) linkCount = 1.
	link uninstall.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = CompiledMethod.
	global removeProperty: #links.


]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnInstanceVariable [
	| ivar link |
	selector := #exampleIvarRead.
	link := MetaLink new.
	ivar := ReflectivityExamples slotNamed: #ivar.
	ivar link: link.
	self assert: (ivar links includes: link).
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	link uninstall.
]

{ #category : #'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnInstanceVariableAndUninstall [
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
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testSetLinkOnPrimitive [
	| methodNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	methodNode := (ReflectivityExamples>>#examplePrimitiveMethod) ast.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode link: link.
	self assert: (methodNode links includes: link).
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self deny: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode removeLink: link.
	(ReflectivityExamples>>#examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) reflectiveMethod isNil.
	
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testSetLinkWithPragmaOptions [
	| sendNode link |
	selector := #exampleMethodWithMetaLinkOptions.
	link := MetaLink new.
	sendNode := (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) class = CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) destroyTwin.
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) reflectiveMethod isNil.
	
]

{ #category : #'tests - links' }
ReflectiveMethodTest >> testSetLinkWithPragmaOptionsPerClass [
	| sendNode link |
	link := MetaLink new.
	sendNode := (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) destroyTwin.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod isNil.
	ReflectivityExamples class recompile: #exampleMethodWithMetaLinkOptionsViaClass
]
