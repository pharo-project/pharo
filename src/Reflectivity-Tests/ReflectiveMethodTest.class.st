Class {
	#name : 'ReflectiveMethodTest',
	#superclass : 'ReflectivityTestCase',
	#instVars : [
		'selector'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
ReflectiveMethodTest >> testASTCacheIsCleanedAfterMetaLinkRemoval [
	| method bcToASTCache |
	selector := #exampleBlock.
	method := ReflectivityExamples >> selector.
	method createTwin.
	bcToASTCache :=  method ast blockNodes first bcToASTCache.

	(ReflectivityExamples >> selector) invalidate.
	ReflectivityExamples new perform: selector.

	self deny: method ast blockNodes first bcToASTCache identicalTo: bcToASTCache
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testCreateReflectiveMethod [
	| compiledMethod reflectiveMethod |
	compiledMethod := ReflectivityExamples >> #exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class equals: ReflectiveMethod
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testCreateReflectiveMethodAndInvalidate [
	| compiledMethod reflectiveMethod |
	selector := #exampleMethod.
	compiledMethod := ReflectivityExamples >> #exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class equals: ReflectiveMethod.
	compiledMethod reflectiveMethod: reflectiveMethod.
	self assert: compiledMethod reflectiveMethod equals: reflectiveMethod.
	compiledMethod invalidate.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod equals: 5.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	(ReflectivityExamples >> #exampleMethod) reflectiveMethod destroyTwin
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testDestroyTwin [	
	| sendNode compiledMethod link |
	selector := #exampleMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples >> #exampleMethod) ast body statements first value.
	sendNode link: link.	
	compiledMethod := (ReflectivityExamples >> #exampleMethod) compiledMethod.
	
	self assert: compiledMethod reflectiveMethod notNil.
	self assert: (ReflectivityExamples >> #exampleMethod) linkCount equals: 1.
	self assert: compiledMethod ast linkCount equals: 1.
	
	compiledMethod destroyTwin.
	
	self assert: (ReflectivityExamples >> #exampleMethod) reflectiveMethod isNil.
	self assert: (ReflectivityExamples >> #exampleMethod) ast linkCount equals: 0
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testInvalidate [
	selector := #exampleMethod.
	(ReflectivityExamples >> #exampleMethod) createTwin.
	(ReflectivityExamples >> #exampleMethod) invalidate.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod equals: 5.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	(ReflectivityExamples >> #exampleMethod) reflectiveMethod destroyTwin
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testInvalidateNoJit [

	(ReflectivityExamples class
	 >> #exampleMethodWithMetaLinkOptionsViaClass) createTwin.
	(ReflectivityExamples class
	 >> #exampleMethodWithMetaLinkOptionsViaClass) invalidate.
	self
		assert: (ReflectivityExamples class
			 >> #exampleMethodWithMetaLinkOptionsViaClass) class
		equals: CompiledMethod.
	self
		assert:
		ReflectivityExamples exampleMethodWithMetaLinkOptionsViaClass
		equals: 5.
	self
		assert: (ReflectivityExamples class
			 >> #exampleMethodWithMetaLinkOptionsViaClass) class
		equals: CompiledMethod.
	(ReflectivityExamples class
	 >> #exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod
		destroyTwin.
	ReflectivityExamples class recompile:
		#exampleMethodWithMetaLinkOptionsViaClass
]

{ #category : 'tests - creation' }
ReflectiveMethodTest >> testInvalidateWithLinkCount [
	selector := #exampleMethod.
	(ReflectivityExamples >> #exampleMethod) createTwin.
	(ReflectivityExamples >> #exampleMethod) invalidate.
	ReflectivityExamples >> #exampleMethod linkCount: 1.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod equals: 5.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleMethod) reflectiveMethod isNil.
	(ReflectivityExamples >> #exampleMethod) reflectiveMethod destroyTwin
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testLinkCountTwoLinks [
	| sendNode link1 link2 |
	selector := #exampleMethod.
	link1 := MetaLink new.
	link2 := MetaLink new.
	sendNode := (ReflectivityExamples >> #exampleMethod) ast body statements first value.
	sendNode link: link1.
	self assert: (ReflectivityExamples >> #exampleMethod) linkCount equals: 1.
	sendNode link: link2.
	self assert: (ReflectivityExamples >> #exampleMethod) linkCount equals: 2.
	sendNode removeLink: link1.
	self assert: (ReflectivityExamples >> #exampleMethod) linkCount equals: 1.
	sendNode removeLink: link2.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	self assert: (ReflectivityExamples >> #exampleMethod) reflectiveMethod isNil
]

{ #category : 'tests' }
ReflectiveMethodTest >> testLinkKeepsExtension [

	[
	| metalink |
	ReflectivityExamples compile: 'exampleWithExtension' classified: '*GeneratedPackageForTest'.

	self assert: (ReflectivityExamples >> #exampleWithExtension) isExtension.

	metalink := MetaLink new.
	(ReflectivityExamples >> #exampleWithExtension) ast link: metalink.

	self assert: (ReflectivityExamples >> #exampleWithExtension) isExtension.

	metalink uninstall.

	self assert: (ReflectivityExamples >> #exampleWithExtension) isExtension ] ensure: [ self packageOrganizer removePackage: 'GeneratedPackageForTest' ]
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLink [
	| sendNode link |
	selector := #exampleMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples >> #exampleMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode hasMetalink: link).
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: ReflectiveMethod.
	sendNode removeLink: link.
	(ReflectivityExamples >> #exampleMethod) destroyTwin.
	self assert: (ReflectivityExamples >> #exampleMethod) class equals: CompiledMethod.
	self assert: (ReflectivityExamples >> #exampleMethod) reflectiveMethod isNil
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkInPrimitive [
	| sendNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples >> #examplePrimitiveMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class equals: CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples >> #examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class equals: CompiledMethod.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) reflectiveMethod isNil.
	(ReflectivityExamples >> #examplePrimitiveMethod) recompile
]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnClassVariable [
	| classVar link |
	selector := #exampleClassVarRead.
	link := MetaLink new.
	classVar := (ReflectivityExamples classVariableNamed: #ClassVar).
	classVar link: link.

	self assert: (classVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class equals: ReflectiveMethod.
	link uninstall.
	classVar removeProperty: #links.

]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnClassVariableAndUninstall [
	| classVar link |
	selector := #exampleClassVarRead.
	link := MetaLink new.
	classVar := (ReflectivityExamples classVariableNamed: #ClassVar).
	classVar link: link.

	self assert: (classVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class equals: ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) linkCount equals: 1.

	link uninstall.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class equals: CompiledMethod
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkOnDynamicArrayArgument [

 	| link |
 	link := MetaLink new
 		        metaObject: #node;
 		        selector: #tagExecuted;
 		        yourself.
 	(ReflectivityExamples >> #exampleDynamicArrayArgument)
 		compiledMethod recompile.
 	ReflectivityExamples new exampleDynamicArrayArgument.
 	(ReflectivityExamples >> #exampleDynamicArrayArgument) ast
 		nodesDo: [ :n |
 			n removeProperty: #tagExecuted ifAbsent: [  ].
 			n isDynamicArray ifTrue: [
 				n
 					propertyAt: #tagExecuted put: false;
 					link: link ] ].
 	ReflectivityExamples new exampleDynamicArrayArgument.
 	link uninstall.
 	(ReflectivityExamples >> #exampleDynamicArrayArgument) ast
 		nodesDo: [ :n |
 			n isDynamicArray ifTrue: [
 				self assert: (n propertyAt: #tagExecuted) ]]
]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnGlobalVariable [
	| globalVar link |
	selector := #exampleGlobalRead.
	link := MetaLink new.
	globalVar := (testingEnvironment at: #GlobalForTesting) binding.
	globalVar link: link.
	self assert: (globalVar links includes: link).
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class equals: ReflectiveMethod.
	link uninstall.
	globalVar removeProperty: #links
]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnGlobalVariableAndUninstall [
	| global link |
	selector := #exampleGlobalRead.
	link := MetaLink new.
	global := (testingEnvironment at: #GlobalForTesting) binding.
	global link: link.
	self assert: (global links includes: link).
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class equals: ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) linkCount equals: 1.
	link uninstall.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class equals: CompiledMethod.
	global removeProperty: #links
]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnInstanceVariable [
	| ivar link |
	selector := #exampleIvarRead.
	link := MetaLink new.
	ivar := ReflectivityExamples slotNamed: #ivar.
	ivar link: link.
	self assert: (ivar links includes: link).
	self assert: (ReflectivityExamples >> #exampleIvarRead) class equals: ReflectiveMethod.
	link uninstall
]

{ #category : 'tests - variables' }
ReflectiveMethodTest >> testSetLinkOnInstanceVariableAndUninstall [
	| ivar link |
	selector := #exampleIvarRead.
	link := MetaLink new.
	ivar := ReflectivityExamples slotNamed: #ivar.
	ivar link: link.
	self assert: (ivar links includes: link).
	self assert: (ReflectivityExamples >> #exampleIvarRead) class equals: ReflectiveMethod.
	self assert: (ReflectivityExamples >> #exampleIvarRead) linkCount equals: 1.
	link uninstall.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class equals: CompiledMethod.
	self assertEmpty: ivar links
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkOnPragma [

 	| link a b |
 	link := MetaLink new
 		        metaObject: #node;
 		        selector: #tagExecuted;
 		        yourself.
 	(ReflectivityExamples >> #examplePragma) compiledMethod recompile.
 	a := ReflectivityExamples new examplePragma.
 	(ReflectivityExamples >> #examplePragma) ast nodesDo: [ :n |
 		n removeProperty: #tagExecuted ifAbsent: [  ].
 		n isPragma ifTrue: [
 			n
 				propertyAt: #tagExecuted put: false;
 				link: link ] ].
 	b := ReflectivityExamples new examplePragma.
 	link uninstall.
 	self assert: a equals: b.
 	(ReflectivityExamples >> #examplePragma) ast nodesDo: [ :n |
 		n isPragma ifTrue: [ self assert: (n propertyAt: #tagExecuted) ] ]
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkOnPrimitive [
	| methodNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	methodNode := (ReflectivityExamples>>#examplePrimitiveMethod) ast.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode link: link.
	self assert: (methodNode links includes: link).
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class equals: CompiledMethod.
	self deny: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode removeLink: link.
	(ReflectivityExamples>>#examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class equals: CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) reflectiveMethod isNil
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkWithPragmaOptions [
	| sendNode link |
	selector := #exampleMethodWithMetaLinkOptions.
	link := MetaLink new.
	sendNode := (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) class equals: CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) destroyTwin.
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) class equals: CompiledMethod.
	self assert: (ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) reflectiveMethod isNil
]

{ #category : 'tests - links' }
ReflectiveMethodTest >> testSetLinkWithPragmaOptionsPerClass [
	| sendNode link |
	link := MetaLink new.
	sendNode := (ReflectivityExamples class >> #exampleMethodWithMetaLinkOptionsViaClass) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples class >> #exampleMethodWithMetaLinkOptionsViaClass) class equals: CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples class >> #exampleMethodWithMetaLinkOptionsViaClass) destroyTwin.
	self assert: (ReflectivityExamples class >> #exampleMethodWithMetaLinkOptionsViaClass) class equals: CompiledMethod.
	self assert: (ReflectivityExamples class >> #exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod isNil.
	ReflectivityExamples class recompile: #exampleMethodWithMetaLinkOptionsViaClass
]
