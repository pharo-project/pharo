Class {
	#name : #RGClassTest,
	#superclass : #RGTest,
	#traits : 'TRGBehaviorTest + TRGClassDescriptionTest',
	#classTraits : 'TRGBehaviorTest classTrait + TRGClassDescriptionTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGClassTest >> behaviorClass [

	^ RGClass
]

{ #category : #tests }
RGClassTest >> testClassSide [
	| behavior |
	behavior := RGClass unnamed.
	self assert: behavior classSide identicalTo: behavior metaclass.
	self assert: behavior metaclass classSide identicalTo: behavior metaclass
]

{ #category : #tests }
RGClassTest >> testClassVariablesCollection [
	
	| class env classVariable1 classVariable2 classVariable3 |

	class := RGClass unnamed.
	env := class environment.

	self assert: class classVariables isEmpty.
	self assert: (class hasUnresolved: #classVariables).
	
	classVariable1 := RGClassVariable named: #ClassVariable1 parent: class.
	class addClassVariable: classVariable1.
	self assert: class classVariables size equals: 1.
	self assert: class allClassVariables size equals: 1.
	self assert: class allClassVarNames equals: #(ClassVariable1).

	self assert: (class hasResolved: #classVariables).
	
	classVariable2 := RGClassVariable named: #ClassVariable2 parent: class.
	class addClassVariable: classVariable2.
	self assert: class classVariables size equals: 2.
	self assert: class allClassVariables size equals: 2.
	self assert: class allClassVarNames equals: #(ClassVariable1 ClassVariable2).
	
	classVariable3 := RGClassVariable named: #ClassVariable3 parent: class.
	class addClassVariable: classVariable3.
	self assert: class classVariables size equals: 3.
	self assert: class allClassVariables size equals: 3.
	self assert: class allClassVarNames equals: #(ClassVariable1 ClassVariable2 ClassVariable3).

	class removeClassVariable: classVariable3.
	self assert: class classVariables size equals: 2.
	self assert: class allClassVariables size equals: 2.
	self assert: class allClassVarNames equals: #(ClassVariable1 ClassVariable2).

	class cleanClassVariables.
	self assert: class classVariables isEmpty.
	self assert: (class hasResolved: #classVariables).

]

{ #category : #tests }
RGClassTest >> testConversionToTrait [

	| env behavior trait | 
	
	"TODO: more complete test"
	
	env := RGEnvironment new.
	behavior := env ensureClassNamed: #SomeTrait.
	self deny: behavior isTrait.
	self assert: behavior isClass.
	
	trait := env ensureTraitNamed: #SomeTrait.
	self deny: behavior isClass.
	self assert: behavior isTrait.
	
]

{ #category : #tests }
RGClassTest >> testCopyForBehaviorDefinitionForClass [
	"test copying for instance variables from RGClass"

	| env aClass copy classVariable1 sharedPool sharedPool2 |
	env := RGEnvironment new.
	aClass := env ensureClassNamed: #SomeClass.
	sharedPool := env ensureClassNamed: #SomePool.
	sharedPool2 := env ensureClassNamed: #SomePool2.

	classVariable1 := RGClassVariable named: #ClassVariable1 parent: aClass.
	aClass addClassVariable: classVariable1.
	aClass addSharedPool: sharedPool.

	self assert: aClass sharedPools anyOne name equals: #SomePool.

	copy := aClass copyForBehaviorDefinition.

	classVariable1 name: #ChangedClassVariable1.
	aClass cleanSharedPools.
	aClass addSharedPool: sharedPool2.

	self deny: copy identicalTo: aClass.
	self assert: copy classVarNames asArray equals: #(ClassVariable1).
	self assert: (copy classVariables allSatisfy: [ :each | each parent == copy ]).

	self assert: copy sharedPools notEmpty.
	self assert: copy sharedPools anyOne name equals: #SomePool
]

{ #category : #tests }
RGClassTest >> testDefinition [
	| env aClass |
	env := RGEnvironment new.

	aClass := env ensureClassNamed: 'Object'.
	aClass superclass: (env ensureClassNamed: 'ProtoObject').
	aClass package: (env ensurePackageNamed: 'Kernel-Objects').
	aClass addClassVariable: (RGClassVariable named: #DependentsFields parent: aClass).

	self
		assert: aClass definition
		equals:
			'ProtoObject subclass: #Object
	instanceVariableNames: ''''
	classVariableNames: ''DependentsFields''
	package: ''Kernel-Objects'''
]

{ #category : #tests }
RGClassTest >> testEnsureProtocol [

	| behavior |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RGClassTest >> testEnvironmentStubs [

	| package env |
	
	package := RGClass named: 'SomeClass'.
	env := package environment.
"	self assert: (env ask behaviors size = (2)). 
	self assert: (env ask packages size = (1+2)).
	self assert: (env ask packages anySatisfy: [ :each | each unresolvedProperties includes: #name]).
"	
]

{ #category : #tests }
RGClassTest >> testPrinting [
	| aClass |
	
	aClass := RGClass named: 'Object'.
	
	self assert: aClass printString equals: #Object.
	
]

{ #category : #tests }
RGClassTest >> testSharedPoolsCollection [
	
	| class env sharedPool1 sharedPool2 sharedPool3 |

	class := RGClass unnamed.
	env := class environment.

	self assert: class sharedPools isEmpty.
	self assert: (class hasUnresolved: #sharedPools).
	
	sharedPool1 := RGPoolVariable named: #SharedPool1 parent: class.
	class addSharedPool: sharedPool1.
	self assert: class sharedPools size equals: 1.
	self deny: sharedPool1 isClassVariable.
	self assert: sharedPool1 isPoolVariable.
	
	self assert: (class hasResolved: #sharedPools).
	
	sharedPool2 := RGPoolVariable named: #SharedPool2 parent: class.
	class addSharedPool: sharedPool2.
	self assert: class sharedPools size equals: 2.
	
	sharedPool3 := RGPoolVariable named: #SharedPool3 parent: class.
	class addSharedPool: sharedPool3.
	self assert: class sharedPools size equals: 3.

	class removeSharedPool: sharedPool3.
	self assert: class sharedPools size equals: 2.

	class cleanSharedPools.
	self assert: class sharedPools isEmpty.
	self assert: (class hasResolved: #sharedPools).

]

{ #category : #tests }
RGClassTest >> testTagsCollection [
	
	| class env tag1 tag2 tag3 |

	class := RGClass unnamed.
	env := class environment.

	self assert: class tags isEmpty.
	self assert: (class hasUnresolved: #tags).
	
	tag1 := #tag1.
	class tagWith: #tag1.
	self assert: class tags size equals: 1.

	self assert: (class hasResolved: #tags).
	
	tag2 := #tag2.
	class tagWith: tag2.
	self assert: class tags size equals: 2.
	
	tag3 := #tag3.
	class tagWith: tag3.
	self assert: class tags size equals: 3.

	class untagFrom: tag3.
	self assert: class tags size equals: 2.

	class cleanTags.
	self assert: class tags isEmpty.
	self assert: (class hasResolved: #tags).

]

{ #category : #tests }
RGClassTest >> testTagsForAllMethods [
	
	| env class1 trait1 trait2 composition method1 method2 method3 method4 |
	
	env := RGEnvironment new.
	class1 := env ensureClassNamed: #Class1.
	composition :=  RGTraitComposition parent: class1.
	class1 traitComposition: composition.
	trait1 := env ensureTraitNamed: #Trait1.
	trait2 := env ensureTraitNamed: #Trait2.
	composition addTransformation: trait1.
	composition addTransformation: trait2.
	
	method1 := class1 ensureLocalMethodNamed: #method1.	
	method1 tagWith: #tag1.
	self assertCollection: class1 tagsForAllMethods hasSameElements: #(#tag1).

	method2 := trait1 ensureLocalMethodNamed: #method2.
	method2 tagWith: #tag1.
	self assertCollection: class1 tagsForAllMethods hasSameElements: #(#tag1).

	method3 := trait1 ensureLocalMethodNamed: #method3.
	method3 tagWith: #tag2.
	self assertCollection: class1 tagsForAllMethods hasSameElements: #(#tag1 tag2).
	
	method4 := trait1 ensureLocalMethodNamed: #method4.
	method4 tagWith: #tag3.
	self assertCollection: class1 tagsForAllMethods hasSameElements: #(#tag1 tag2 tag3).
	



]

{ #category : #tests }
RGClassTest >> testTagsForAllMethodsForClassWithoutTraits [
	
	| class method1 method2 |

	class := RGClass unnamed.
	
	self assert: class tagsForAllMethods isEmpty.
	
	method1 := class ensureLocalMethodNamed: #method1.
	method2 := class ensureLocalMethodNamed: #method2.
	
	self assert: class tagsForMethods isEmpty.
	
	method1 tagWith: #tag1.
	self assertCollection: class tagsForAllMethods hasSameElements: #(#tag1).
	
	method2 tagWith: #tag1.
	self assertCollection: class tagsForAllMethods hasSameElements: #(#tag1).

	method1 tagWith: #tag2.
	self assertCollection: method1 tags sorted hasSameElements: #(#tag1 #tag2).
	self assertCollection: class tagsForMethods sorted hasSameElements: #(#tag1 #tag2).
	
]

{ #category : #tests }
RGClassTest >> testTagsForMethods [
	
	| class method1 method2 |

	class := RGClass unnamed.
	
	self assert: class tagsForMethods isEmpty.
	self assert: (class hasUnresolved: #tagsForMethods).
	
	method1 := class ensureLocalMethodNamed: #method1.
	method2 := class ensureLocalMethodNamed: #method2.
	
	self assert: class tagsForMethods isEmpty.
	self assert: (class hasResolved: #tagsForMethods).

	self assert: (class methodsTaggedWith: #tag1) isEmpty.
	
	method1 tagWith: #tag1.
	self assert: method1 tags equals: #(#tag1).
	self assert: class tagsForMethods equals: #(#tag1).
	self assert: (class methodsTaggedWith: #tag1) equals: {method1}.
	
	method2 tagWith: #tag1.
	self assert: method2 tags equals: #(#tag1).
	self assert: class tagsForMethods equals: #(#tag1).
	self assert: ((class methodsTaggedWith: #tag1) includesAll: {method1. method2}).

	method1 tagWith: #tag2.
	self assert: method1 tags sorted equals: #(#tag1 #tag2).
	self assert: class tagsForMethods sorted equals: #(#tag1 #tag2).
	
	method1 untagFrom: #tag1.
	self assert: method1 tags equals: #(#tag2).
	self assert: class tagsForMethods sorted equals: #(#tag1 #tag2).
	self assert: ((class methodsTaggedWith: #tag1) includesAll: {method2}).

	method2 tagWith: #tag3.
	self assert: method2 tags sorted equals: #(#tag1 #tag3).
	self assert: class tagsForMethods sorted equals: #(#tag1 #tag2 #tag3).

	class removeLocalMethod: method1.
	self assert: class tagsForMethods sorted equals: #(#tag1 #tag2 #tag3).
	class removeLocalMethod: method2.
	self assert: class tagsForMethods sorted equals: #(#tag1 #tag2 #tag3).

	self assert: (class methodsTaggedWith: #tag1) isEmpty.
	self assert: (class methodsTaggedWith: #tag2) isEmpty.
	self assert: (class methodsTaggedWith: #tag3) isEmpty.

]

{ #category : #tests }
RGClassTest >> testTagsForMethodsCollection [
	
	| class env tag1 tag2 tag3 |

	class := RGClass unnamed.
	env := class environment.

	self assert: class tagsForMethods isEmpty.
	self assert: (class hasUnresolved: #tagsForMethods).
	
	tag1 := #tag1.
	class addMethodTag: #tag1.
	self assert: class tagsForMethods size equals: 1.

	self assert: (class hasResolved: #tagsForMethods).
	
	tag2 := #tag2.
	class addMethodTag: tag2.
	self assert: class tagsForMethods size equals: 2.
	
	tag3 := #tag3.
	class addMethodTag: tag3.
	self assert: class tagsForMethods size equals: 3.

	class removeMethodTag: tag3.
	self assert: class tagsForMethods size equals: 2.

	class cleanTagsForMethods.
	self assert: class tagsForMethods isEmpty.
	self assert: (class hasResolved: #tagsForMethods).

]

{ #category : #tests }
RGClassTest >> testTagsForMethodsRemoval [
	
	| class method1 method2 |

	class := RGClass unnamed.
	
	self assert: class tagsForMethods isEmpty.
	self assert: (class hasUnresolved: #tagsForMethods).
	
	method1 := class ensureLocalMethodNamed: #method1.
	method2 := class ensureLocalMethodNamed: #method2.
	
	self assert: class tagsForMethods isEmpty.
	self assert: (class hasResolved: #tagsForMethods).

	method1 tagWith: #tag1.
	self assert: method1 tags equals: #(#tag1).
	self assert: class tagsForMethods equals: #(#tag1).
	
	class removeMethodTag: #tag1.
	self assert: method1 tags isEmpty.
	self assert: class tagsForMethods isEmpty.

]

{ #category : #tests }
RGClassTest >> testTesting [

	| env behavior | 
	
	"TODO: more complete test"
	
	env := RGEnvironment new.
	behavior := env ensureClassNamed: #SomeTrait.
	self deny: behavior isBits.
	self deny: behavior isBytes.
	self deny: behavior isCompiledMethod.
	self deny: behavior isEphemeron.
	self deny: behavior isWeak.
	self deny: behavior isWords.
	self assert: behavior isPointers.

]
