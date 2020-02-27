Class {
	#name : #RGEnvironmentBackendTest,
	#superclass : #RGBackendTest,
	#traits : 'TRGReadOnlyTest',
	#classTraits : 'TRGReadOnlyTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #'as yet unclassified' }
RGEnvironmentBackendTest >> backendClass [

	^ RGEnvironmentBackend
]

{ #category : #'as yet unclassified' }
RGEnvironmentBackendTest >> checkHierarchyConsistencyOf: anRGClass [
	self assert: anRGClass ~~ anRGClass superclass.
	self assert: anRGClass ~~ anRGClass metaclass.
	self assert: anRGClass ~~ anRGClass superclass metaclass.

	self assert: anRGClass superclass ~~ anRGClass metaclass.
	self assert: anRGClass superclass ~~ anRGClass superclass metaclass.
	self assert: anRGClass metaclass ~~ anRGClass superclass metaclass.

	self assert: anRGClass superclass superclass identicalTo: anRGClass superclass.
	self assert: anRGClass superclass metaclass identicalTo: anRGClass metaclass superclass.

	self assert: anRGClass superclass metaclass superclass identicalTo: anRGClass superclass metaclass.
	self assert: anRGClass superclass metaclass metaclass identicalTo: anRGClass superclass metaclass
]

{ #category : #tests }
RGEnvironmentBackendTest >> testAST [

	| method anAst |
	
	method := RGMethod new.
	method sourceCode: 'someMethod ^ true'.
	anAst := method ast.
	self assert: (anAst isMethod).
	self assert: (anAst lastIsReturn)


]

{ #category : #tests }
RGEnvironmentBackendTest >> testCreationOfDefinitions [
	| env1 method |
	env1 := RGEnvironment new.

	"self should: [ (Object>>#isRingObject) asRingDefinitionIn: env1 ] raise: RGNotFoundError."
	method := Object >> #isRGObject ensureRingDefinitionIn: env1.
	self assert: method class equals: RGMethod.
	self assert: method selector equals: #isRGObject.
	self assert: method parent name equals: #Object.
	self assert: method environment identicalTo: env1
]

{ #category : #tests }
RGEnvironmentBackendTest >> testDefaultEnvironmentContent [
	
	| env |
	
	env := RGEnvironment unnamed.
	
	self testDefaultContentFor: env.
	
"	self assert: env ask behaviors size equals: 12.
	self assert: env ask packages size equals: 1.
"
]

{ #category : #tests }
RGEnvironmentBackendTest >> testDefinitionsIdentity [
	| env1 |
	env1 := RGEnvironment new.

	self assert: (Object >> #isRGObject ensureRingDefinitionIn: env1) identicalTo: (Object >> #isRGObject ensureRingDefinitionIn: env1).

	self assert: (Object >> #isRGObject ensureRingDefinitionIn: env1) parent identicalTo: (Object >> #isRGObject ensureRingDefinitionIn: env1) parent
]

{ #category : #tests }
RGEnvironmentBackendTest >> testImplicitEnvironmentContentForClassDefinition [
	
	| def def2 env |

	def := RGClass named: #SomeClass.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	self checkHierarchyConsistencyOf: def.
	
	self assert: (def isRingResolved).
	self deny: (def superclass isRingResolved).
	self deny: (def metaclass isRingResolved).
	self deny: (def superclass metaclass isRingResolved).
	
	def2 := RGClass named: #SomeOtherClass parent: env.
	self assert: env ask behaviors size equals: 8. 
	self assert: env ask packages size equals: 4.
	
	self checkHierarchyConsistencyOf: def2.

	self assert: (def2 isRingResolved).
	self deny: (def2 superclass isRingResolved).
	self deny: (def2 metaclass isRingResolved).
	self deny: (def2 superclass metaclass isRingResolved).
	

]

{ #category : #tests }
RGEnvironmentBackendTest >> testImplicitEnvironmentContentForMetaclassDefinition [
	
	| def env baseClass|

	def := RGMetaclass named: #'SomeClass class'.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	baseClass := def baseClass.
	self checkHierarchyConsistencyOf: baseClass.
	
	self deny: (baseClass isRingResolved).
	self deny: (baseClass superclass isRingResolved).
	self assert: (baseClass metaclass isRingResolved).
	self deny: (baseClass superclass metaclass isRingResolved).
	

]

{ #category : #tests }
RGEnvironmentBackendTest >> testImplicitEnvironmentContentForMethodDefinition [
	
	| def env |

	def := RGMethod named: #someMethod.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	self checkHierarchyConsistencyOf: def parent.
	
	self deny: (def parent isRingResolved).
	self deny: (def parent superclass isRingResolved).
	self deny: (def parent metaclass isRingResolved).
	self deny: (def parent superclass metaclass isRingResolved).
	

]
