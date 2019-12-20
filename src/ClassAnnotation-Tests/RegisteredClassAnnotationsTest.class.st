Class {
	#name : #RegisteredClassAnnotationsTest,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #running }
RegisteredClassAnnotationsTest >> setUp [
	super setUp.
	
	ClassAnnotation resetCache
]

{ #category : #running }
RegisteredClassAnnotationsTest >> tearDown [
		
	ClassAnnotation resetCache.
	
	super tearDown.
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testAreCached [
	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.

	self assert: ClassAnnotationExample1 registeredInstances identicalTo: annotations
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testAreDifferentForDifferentAnnotationClasses [
	| annotations1 annotations2 |
	annotations1 := ClassAnnotationExample1 registeredInstances.
	annotations2 := ClassAnnotationExample2 registeredInstances.

	self deny: annotations1 equals: annotations2
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testAreEmptyForUnusedAnnotation [

	| actual |
	actual := ClassAnnotationUnusedExample registeredInstances.
	
	self assert: actual isEmpty
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testAreSortedByPriority [

	| annotations expected |
	annotations := ClassAnnotationExampleWithPriority registeredInstances.
	
	self assert: annotations size equals: 3.
	
	expected := annotations sorted: #priority descending.
	self assert: annotations asArray equals: expected asArray
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testCanBeEnumerated [

	| enumerated |
	enumerated := OrderedCollection new.
	ClassAnnotationExample1 registeredInstancesDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassAnnotationExample1 registeredInstances asArray
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testCanBeQueriedForConcreteClass [

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstancesFor: ClassWithThreeAnnotations.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #annotatedClass as: Set) equals: {ClassWithThreeAnnotations} asSet
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testIncludeCopiesForEachSubclassOfOriginDeclaration [

	| annotations |
	annotations := ClassAnnotationExample3 registeredInstances.
	
	self assert: (annotations anySatisfy: [ :each | each annotatedClass = SubclassWithOnlyInheritedAnnotation ])
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testIncludeForbiddenClassesAsSpecialForbiddenAnnotations [
	| allForbidden forbidden actualAnnotation |
	
	allForbidden := ForbiddenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	forbidden := allForbidden detect: [:each | each declarationSelector = #forbiddenAnnotation].
	actualAnnotation := forbidden actualAnnotation.
	self assert: actualAnnotation class equals: ForbiddenClassAnnotationExample
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testIncludeInstancesFromAllAnnotatingMethodsOfAnyClassInSystem [

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #class as: Set) equals: {ClassAnnotationExample1} asSet
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testNotIncludeForbiddenClassesAsBrokenAnnotations [
	| broken |
	
	broken := BrokenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	broken 
		detect: [:each | each declarationSelector = #forbiddenAnnotation]
		ifFound: [ :ann | self assert: false description: 'forbidden annotation should not be broken' ]
]

{ #category : #tests }
RegisteredClassAnnotationsTest >> testNotIncludeInstancesAnnotatingForbiddenClasses [
	"In that test the annotation define canAnnotateClass: with constant false answer.
	So any annotating methods which define it should never register their instances"
	| annotations |
	annotations := ForbiddenClassAnnotationExample registeredInstances.
	
	self assert: annotations isEmpty
]
