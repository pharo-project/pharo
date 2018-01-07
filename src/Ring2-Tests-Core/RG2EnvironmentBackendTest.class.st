Class {
	#name : #RG2EnvironmentBackendTest,
	#superclass : #RG2BackendTest,
	#traits : 'TRG2ReadOnlyDefinitionTest',
	#classTraits : 'TRG2ReadOnlyDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2EnvironmentBackendTest >> backendClass [

	^ RG2EnvironmentBackend
]

{ #category : #'as yet unclassified' }
RG2EnvironmentBackendTest >> checkHierarchyConsistencyOf: aRG2ClassDefinition [

	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition superclass).
	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition metaclass).
	self assert: (aRG2ClassDefinition ~~ aRG2ClassDefinition superclass metaclass).

	self assert: (aRG2ClassDefinition superclass ~~ aRG2ClassDefinition metaclass).
	self assert: (aRG2ClassDefinition superclass ~~ aRG2ClassDefinition superclass metaclass).
	self assert: (aRG2ClassDefinition metaclass ~~ aRG2ClassDefinition superclass metaclass).

	self assert: (aRG2ClassDefinition superclass superclass == aRG2ClassDefinition superclass).
	self assert: (aRG2ClassDefinition superclass metaclass == aRG2ClassDefinition metaclass superclass).

	self assert: (aRG2ClassDefinition superclass metaclass superclass == aRG2ClassDefinition superclass metaclass).
	self assert: (aRG2ClassDefinition superclass metaclass metaclass == aRG2ClassDefinition superclass metaclass).
	

]

{ #category : #tests }
RG2EnvironmentBackendTest >> testAST [

	| method anAst |
	
	method := RG2MethodDefinition new.
	method sourceCode: 'someMethod ^ true'.
	anAst := method ast.
	self assert: (anAst isMethod).
	self assert: (anAst lastIsReturn)


]

{ #category : #tests }
RG2EnvironmentBackendTest >> testCreationOfDefinitions [

	| env1 method |
	
	env1 := RG2Environment new.
	
	"self should: [ (Object>>#isRing2Object) asRing2DefinitionIn: env1 ] raise: RG2NotFoundError."
	method := (Object>>#isRing2Object) ensureRing2DefinitionIn: env1.
	self assert: method class equals: RG2MethodDefinition.	
	self assert: method selector equals: #isRing2Object.
	self assert: method parent name equals: #Object.
	self assert: (method environment == env1).
	


]

{ #category : #tests }
RG2EnvironmentBackendTest >> testDefaultEnvironmentContent [
	
	| env |
	
	env := RG2Environment unnamed.
	
	self testDefaultContentFor: env.
	
"	self assert: env ask behaviors size equals: 12.
	self assert: env ask packages size equals: 1.
"
]

{ #category : #tests }
RG2EnvironmentBackendTest >> testDefinitionsIdentity [

	| env1 |
	
	env1 := RG2Environment new.
	
	self assert: (((Object>>#isRing2Object) ensureRing2DefinitionIn: env1) == ((Object>>#isRing2Object) ensureRing2DefinitionIn: env1)).
	
	self assert: (((Object>>#isRing2Object) ensureRing2DefinitionIn: env1) parent
== ((Object>>#isRing2Object) ensureRing2DefinitionIn: env1) parent).
]

{ #category : #tests }
RG2EnvironmentBackendTest >> testImplicitEnvironmentContentForClassDefinition [
	
	| def def2 env |

	def := RG2ClassDefinition named: #SomeClass.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	self checkHierarchyConsistencyOf: def.
	
	self assert: (def isRing2Resolved).
	self deny: (def superclass isRing2Resolved).
	self deny: (def metaclass isRing2Resolved).
	self deny: (def superclass metaclass isRing2Resolved).
	
	def2 := RG2ClassDefinition named: #SomeOtherClass parent: env.
	self assert: env ask behaviors size equals: 8. 
	self assert: env ask packages size equals: 4.
	
	self checkHierarchyConsistencyOf: def2.

	self assert: (def2 isRing2Resolved).
	self deny: (def2 superclass isRing2Resolved).
	self deny: (def2 metaclass isRing2Resolved).
	self deny: (def2 superclass metaclass isRing2Resolved).
	

]

{ #category : #tests }
RG2EnvironmentBackendTest >> testImplicitEnvironmentContentForMetaclassDefinition [
	
	| def def2 env baseClass|

	def := RG2MetaclassDefinition named: #'SomeClass class'.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	baseClass := def baseClass.
	self checkHierarchyConsistencyOf: baseClass.
	
	self deny: (baseClass isRing2Resolved).
	self deny: (baseClass superclass isRing2Resolved).
	self assert: (baseClass metaclass isRing2Resolved).
	self deny: (baseClass superclass metaclass isRing2Resolved).
	

]

{ #category : #tests }
RG2EnvironmentBackendTest >> testImplicitEnvironmentContentForMethodDefinition [
	
	| def def2 env |

	def := RG2MethodDefinition named: #someMethod.
	env := def environment.

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: env ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: env ask packages size equals: 2.
	self assert: (env ask behaviors select: #isMeta) size equals: 2.

	self checkHierarchyConsistencyOf: def parent.
	
	self deny: (def parent isRing2Resolved).
	self deny: (def parent superclass isRing2Resolved).
	self deny: (def parent metaclass isRing2Resolved).
	self deny: (def parent superclass metaclass isRing2Resolved).
	

]
