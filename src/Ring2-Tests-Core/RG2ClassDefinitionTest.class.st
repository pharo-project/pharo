Class {
	#name : #RG2ClassDefinitionTest,
	#superclass : #RG2Test,
	#traits : 'TRG2BehaviorDefinitionTest + TRG2ClassDescriptionDefinitionTest',
	#classTraits : 'TRG2BehaviorDefinitionTest classTrait + TRG2ClassDescriptionDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2ClassDefinitionTest >> behaviorClass [

	^ RG2ClassDefinition
]

{ #category : #tests }
RG2ClassDefinitionTest >> testClassSide [
	
	| behavior |
	behavior := RG2ClassDefinition unnamed.
	self assert: behavior classSide == behavior metaclass. 
	self assert: behavior metaclass classSide == behavior metaclass. 
	
]

{ #category : #tests }
RG2ClassDefinitionTest >> testClassVariablesCollection [
	
	| class env classVariable1 classVariable2 classVariable3 |

	class := RG2ClassDefinition unnamed.
	env := class environment.

	self assert: class classVariables isEmpty.
	self assert: (class hasUnresolved: #classVariables).
	
	classVariable1 := RG2ClassVariableDefinition named: #ClassVariable1 parent: class.
	class addClassVariable: classVariable1.
	self assert: class classVariables size equals: 1.
	self assert: class allClassVariables size equals: 1.
	self assert: class allClassVarNames equals: #(ClassVariable1).

	self assert: (class hasResolved: #classVariables).
	
	classVariable2 := RG2ClassVariableDefinition named: #ClassVariable2 parent: class.
	class addClassVariable: classVariable2.
	self assert: class classVariables size equals: 2.
	self assert: class allClassVariables size equals: 2.
	self assert: class allClassVarNames equals: #(ClassVariable1 ClassVariable2).
	
	classVariable3 := RG2ClassVariableDefinition named: #ClassVariable3 parent: class.
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
RG2ClassDefinitionTest >> testConversionToTrait [

	| env behavior trait | 
	
	"TODO: more complete test"
	
	env := RG2Environment new.
	behavior := env ensureClassNamed: #SomeTrait.
	self deny: behavior isTrait.
	self assert: behavior isClass.
	
	trait := env ensureTraitNamed: #SomeTrait.
	self deny: behavior isClass.
	self assert: behavior isTrait.
	
]

{ #category : #tests }
RG2ClassDefinitionTest >> testCopyForBehaviorDefinitionForClass [

	"test copying for instance variables from RG2ClassDefinition"
	
	| env aClass copy classVariable1 sharedPool sharedPool2 |
	
	env := RG2Environment new.
	aClass := env ensureClassNamed: #SomeClass.
	sharedPool := env ensureClassNamed: #SomePool.
	sharedPool2 := env ensureClassNamed: #SomePool2.

	classVariable1 := RG2ClassVariableDefinition named: #ClassVariable1 parent: aClass.
	aClass addClassVariable: classVariable1.
	aClass addSharedPool: sharedPool.

	self assert: aClass sharedPools anyOne name equals: #SomePool.
		
	copy := aClass copyForBehaviorDefinition.

	classVariable1 name: #ChangedClassVariable1.
	aClass cleanSharedPools.
	aClass addSharedPool: sharedPool2.
	
	self deny: copy == aClass. 
	self assert: copy classVarNames asArray equals: #(ClassVariable1).
	self assert: (copy classVariables allSatisfy: [ :each | each parent == copy ]).
	
	self assert: copy sharedPools notEmpty.
	self assert: copy sharedPools anyOne name equals: #SomePool.
]

{ #category : #tests }
RG2ClassDefinitionTest >> testCorrespondingObject [

	| def1 def2 def3 |
	
	def1 := RG2ClassDefinition named: #SomeClass.
	def2 := RG2ClassDefinition named: #SomeClass.
	def3 := RG2ClassDefinition named: #SomeClass1.
	
	self assert: (def1 correspondingObjectIn: def2 environment) equals: def2.
	self assert: (def1 correspondingObjectIn: def3 environment) equals: nil.
	
	self assert: (def1 superclass correspondingObjectIn: def2 environment) equals: def2 superclass.
	self assert: (def1 metaclass correspondingObjectIn: def2 environment) equals: def2 metaclass.
	self assert: (def1 superclass metaclass correspondingObjectIn: def2 environment) equals: def2 superclass metaclass.

	
		
]

{ #category : #tests }
RG2ClassDefinitionTest >> testCorrespondingObjectForMetaclass [

	| def1 def2 def3 |
	
	def1 := RG2MetaclassDefinition named: 'SomeClass class'.
	def2 := RG2MetaclassDefinition named: 'SomeClass class'.
	def3 := RG2MetaclassDefinition named: 'SomeClass1 class'.
	
	self assert: (def1 correspondingObjectIn: def2 environment) equals: def2.
	self assert: (def1 correspondingObjectIn: def3 environment) equals: nil.

	"TODO: FIX!!!"
	self assert: (def1 baseClass correspondingObjectIn: def2 environment) equals: def2 baseClass.
	self assert: (def1 superclass correspondingObjectIn: def2 environment) equals: def2 superclass.
	self assert: (def1 metaclass correspondingObjectIn: def2 environment) equals: def2 metaclass.
	self assert: (def1 superclass metaclass correspondingObjectIn: def2 environment) equals: def2 superclass metaclass.
	self assert: (def1 superclass metaclass correspondingObjectIn: def2 environment) equals: def2 superclass metaclass.

]

{ #category : #tests }
RG2ClassDefinitionTest >> testCorrespondingObjectForMetaclassTrait [

	| def1 def2 def3 |
	
	def1 := RG2MetaclassTraitDefinition named: 'SomeTrait classTrait'.
	def2 := RG2MetaclassTraitDefinition named: 'SomeTrait classTrait'.
	def3 := RG2MetaclassTraitDefinition named: 'SomeTrait1 classTrait'.
	
	self assert: (def1 correspondingObjectIn: def2 environment) equals: def2.
	self assert: (def1 correspondingObjectIn: def3 environment) equals: nil.

	self assert: (def1 baseTrait correspondingObjectIn: def2 environment) equals: def2 baseTrait.

	self assert: (def1 superclass correspondingObjectIn: def2 environment) equals: def2 superclass.
	self assert: (def1 metaclass correspondingObjectIn: def2 environment) equals: def2 metaclass.
	self assert: (def1 superclass metaclass correspondingObjectIn: def2 environment) equals: def2 superclass metaclass.

]

{ #category : #tests }
RG2ClassDefinitionTest >> testCorrespondingObjectForTrait [

	| def1 def2 def3 |
	
	def1 := RG2TraitDefinition named: 'SomeTrait'.
	def2 := RG2TraitDefinition named: 'SomeTrait'.
	def3 := RG2TraitDefinition named: 'SomeTrait1'.
	
	self assert: (def1 correspondingObjectIn: def2 environment) equals: def2.
	self assert: (def1 correspondingObjectIn: def3 environment) equals: nil.

	self assert: (def1 classTrait correspondingObjectIn: def2 environment) equals: def2 classTrait.

	self assert: (def1 superclass correspondingObjectIn: def2 environment) equals: def2 superclass.
	self assert: (def1 metaclass correspondingObjectIn: def2 environment) equals: def2 metaclass.
	self assert: (def1 superclass metaclass correspondingObjectIn: def2 environment) equals: def2 superclass metaclass.

]

{ #category : #tests }
RG2ClassDefinitionTest >> testDefinition [

	| env aClass |
	
	env := RG2Environment new.
		
	aClass := env ensureClassNamed: 'Object'.
	aClass superclass: (env ensureClassNamed: 'ProtoObject').
	aClass package: (env ensurePackageNamed: 'Kernel-Objects').
	aClass addClassVariable: (RG2ClassVariableDefinition named: #DependentsFields parent: aClass).
	
	self assert: aClass definition = 'ProtoObject subclass: #Object
	instanceVariableNames: ''''
	classVariableNames: ''DependentsFields''
	package: ''Kernel-Objects'''

]

{ #category : #tests }
RG2ClassDefinitionTest >> testEnsureProtocol [

	| behavior protocol |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RG2ClassDefinitionTest >> testEnvironmentStubs [

	| package env |
	
	package := RG2ClassDefinition named: 'SomeClass'.
	env := package environment.
"	self assert: (env ask behaviors size = (2)). 
	self assert: (env ask packages size = (1+2)).
	self assert: (env ask packages anySatisfy: [ :each | each unresolvedProperties includes: #name]).
"	
]

{ #category : #tests }
RG2ClassDefinitionTest >> testPrinting [
	| aClass |
	
	aClass := RG2ClassDefinition named: 'Object'.
	
	self assert: aClass printString equals: #Object.
	
]

{ #category : #tests }
RG2ClassDefinitionTest >> testSharedPoolsCollection [
	
	| class env sharedPool1 sharedPool2 sharedPool3 |

	class := RG2ClassDefinition unnamed.
	env := class environment.

	self assert: class sharedPools isEmpty.
	self assert: (class hasUnresolved: #sharedPools).
	
	sharedPool1 := RG2PoolVariableDefinition named: #SharedPool1 parent: class.
	class addSharedPool: sharedPool1.
	self assert: class sharedPools size equals: 1.
	self deny: sharedPool1 isClassVariable.
	self assert: sharedPool1 isPoolVariable.
	
	self assert: (class hasResolved: #sharedPools).
	
	sharedPool2 := RG2PoolVariableDefinition named: #SharedPool2 parent: class.
	class addSharedPool: sharedPool2.
	self assert: class sharedPools size equals: 2.
	
	sharedPool3 := RG2PoolVariableDefinition named: #SharedPool3 parent: class.
	class addSharedPool: sharedPool3.
	self assert: class sharedPools size equals: 3.

	class removeSharedPool: sharedPool3.
	self assert: class sharedPools size equals: 2.

	class cleanSharedPools.
	self assert: class sharedPools isEmpty.
	self assert: (class hasResolved: #sharedPools).

]

{ #category : #tests }
RG2ClassDefinitionTest >> testTagsCollection [
	
	| class env tag1 tag2 tag3 |

	class := RG2ClassDefinition unnamed.
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
RG2ClassDefinitionTest >> testTagsForMethods [
	
	| class method1 method2 |

	class := RG2ClassDefinition unnamed.
	
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
RG2ClassDefinitionTest >> testTagsForMethodsCollection [
	
	| class env tag1 tag2 tag3 |

	class := RG2ClassDefinition unnamed.
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
RG2ClassDefinitionTest >> testTagsForMethodsRemoval [
	
	| class method1 method2 |

	class := RG2ClassDefinition unnamed.
	
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
RG2ClassDefinitionTest >> testTesting [

	| env behavior | 
	
	"TODO: more complete test"
	
	env := RG2Environment new.
	behavior := env ensureClassNamed: #SomeTrait.
	self deny: behavior isBits.
	self deny: behavior isBytes.
	self deny: behavior isCompiledMethod.
	self deny: behavior isEphemeron.
	self deny: behavior isWeak.
	self deny: behavior isWords.
	self assert: behavior isPointers.

]
