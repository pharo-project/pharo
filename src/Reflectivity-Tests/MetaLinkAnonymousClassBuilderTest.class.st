Class {
	#name : 'MetaLinkAnonymousClassBuilderTest',
	#superclass : 'TestCase',
	#instVars : [
		'builder',
		'object'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MetaLinkAnonymousClassBuilderTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	builder := MetaLinkAnonymousClassBuilder new.
	object := ReflectivityExamples new
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testAllSubclassesOfWithSelector [
	| anonClass1 anonClass2 compiledMethods |
	anonClass1 := builder newAnonymousSubclassFor: ReflectivityExamples.
	anonClass2 := builder newAnonymousSubclassFor: ReflectivityExamples.

	anonClass1 compile: 'exampleAssignment ^self'.
	compiledMethods := builder
		allSubclassesOf: ReflectivityExamples
		withSelector: #exampleAssignment.

	self assert: compiledMethods size equals: 1.
	self assert: compiledMethods asArray first identicalTo: anonClass1
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testAnonSubclassesRegistering [
	|subclasses|
	subclasses := WeakSet with: (builder newAnonymousSubclassFor: ReflectivityExamples).
	self assertCollection: (builder anonSubclassesFor: ReflectivityExamples) equals: subclasses
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testAnonymousClassForObject [

	|originalClass anonClass|
	originalClass := object class.
	anonClass := builder anonymousClassForObject: object.

	self assert: anonClass superclass identicalTo: originalClass.
	self assert: anonClass isAnonymous.
	self assert: (builder anonymousClassForObject: anonClass new) identicalTo: anonClass
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testClassAccessFromClassBuilder [
	|class subclass realClass|
	class := object class.
	subclass := builder newAnonymousSubclassFor: class.
	builder migrateObject: object toAnonymousClass: subclass.

	"Anonymous class is hidden from the system: `object class` returns the original class"
	self assert: object class identicalTo: class.

	"The metalink class builder, however, sees the anonymous class when calling `object class`"
	realClass := object realClass.
	self assert: realClass identicalTo: subclass.
	self assert: (builder requestClassOfObject: object) identicalTo: realClass
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testCompileClassAccessorForAnonymousClass [

	| class |
	class := ReflectivityExamples newAnonymousSubclass.
	builder compileClassAccessorForAnonymousClass: class.
	
	self
		assert: (class compiledMethodAt: #class) sourceCode
		equals: builder classAccessorsForAnonymousClasses first.
	self
		assert: (class compiledMethodAt: #originalClass) sourceCode
		equals: builder classAccessorsForAnonymousClasses second.
	self
		assert: (class compiledMethodAt: #realClass) sourceCode
		equals: builder classAccessorsForAnonymousClasses third
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testCompileMethodFromIn [
	|node anonClass compiledMethod|
	node := (ReflectivityExamples >> #exampleAssignment) ast statements first.
	anonClass := ReflectivityExamples newAnonymousSubclass.
	compiledMethod := builder compileMethodFrom: node in: anonClass.

	self assert: compiledMethod methodClass identicalTo: anonClass.
	self assert: compiledMethod sourceCode equals: node methodNode sourceCode
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testCompiledMethodsOfSelectorInAnonSubClassesOf [
	| anonClass compiledMethods |
	anonClass := builder newAnonymousSubclassFor: ReflectivityExamples.
	anonClass compile: 'exampleAssignment ^self'.
	compiledMethods := builder
		compiledMethodsOfSelector: #exampleAssignment
		inAnonSubClassesOf: ReflectivityExamples.

	self assert: compiledMethods size equals: 1.
	self assert: compiledMethods asArray first methodClass identicalTo: anonClass
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testCompiledMethodsOfSelectorinClasses [
	| anonClass1 anonClass2 compiledMethods |
	anonClass1 := ReflectivityExamples newAnonymousSubclass.
	anonClass2 := ReflectivityExamples newAnonymousSubclass.

	anonClass1 compile: 'exampleAssignment ^self'.
	compiledMethods := builder
		compiledMethodsOfSelector: #exampleAssignment
		inClasses: {anonClass1. anonClass2}.

	self assert: compiledMethods size equals: 1.
	self assert: compiledMethods asArray first methodClass identicalTo: anonClass1
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testEmptyAnonSubclasses [
	self assertCollection: (builder anonSubclassesFor: ReflectivityExamples) equals: Array new
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testMigrateObjectToAnonymousClass [
	|class subclass realClass originalClass currentClass|
	class := object class.
	subclass := builder newAnonymousSubclassFor: class.
	builder migrateObject: object toAnonymousClass: subclass.

	realClass := object realClass.
	self assert: realClass identicalTo: subclass.
	self assert: (builder soleInstanceOf: realClass) identicalTo: object.

	"originalClass is defined in #classAccessorsForAnonymousClasses"
	originalClass := object perform: #originalClass.
	currentClass := object class.
	self assert: currentClass identicalTo: class.
	self assert: currentClass identicalTo: originalClass
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testMigrateObjectToOriginalClass [
	|class subclass|
	class := object class.
	subclass := builder newAnonymousSubclassFor: class.
	builder migrateObject: object toAnonymousClass: subclass.
	builder migrateObjectToOriginalClass: object.

	self should: [ object realClass ] raise: MessageNotUnderstood.
	self should: [ object perform: #originalClass ] raise: MessageNotUnderstood.
	self assert: object class identicalTo: class
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testNewAnonymousSubclass [
	|class|
	class := (builder newAnonymousSubclassFor: ReflectivityExamples).
	self assert: class isAnonymous.
	self assert: class superclass identicalTo: ReflectivityExamples.
	self assert: class new class identicalTo: ReflectivityExamples
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testRemoveMethodNodeFromObject [
	|node anonClass|
	node := (object class >> #exampleAssignment) ast.

	builder removeMethodNode: node fromObject: object.
	self deny: object class isAnonymous.
	self assertCollection: object class selectors includesAll: {#exampleAssignment}.

	anonClass := object class newAnonymousSubclass.
	anonClass compile: 'exampleAssignment ^self'.
	anonClass adoptInstance: object.

	self assert: object class isAnonymous.
	self assertCollection: object class selectors includesAll: {#exampleAssignment}.

	builder removeMethodNode: node fromObject: object.
	self denyCollection: object class selectors includesAll: {#exampleAssignment}
]

{ #category : 'tests' }
MetaLinkAnonymousClassBuilderTest >> testWeakMigratedObjectsRegistry [
	|originalClass anonClass|
	originalClass := object class.
	anonClass := builder anonymousClassForObject: object.
	builder migrateObject: object toAnonymousClass: anonClass.

	self assert: (builder soleInstanceOf: anonClass) identicalTo: object.
	self assertCollection: (builder anonSubclassesFor: originalClass) includesAll: {anonClass}.

	object := nil.
	Smalltalk garbageCollect.
	self should: [builder soleInstanceOf: anonClass] raise: ValueNotFound.

	anonClass := nil.
	Smalltalk garbageCollect.
	self assert: (builder migratedObjects allSatisfy: [:e| e isNil]).
	self should: [builder soleInstanceOf: nil] raise: KeyNotFound
]
