Class {
	#name : #RegisteredClassAnnotationsTests,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #running }
RegisteredClassAnnotationsTests >> setUp [
	super setUp.
	
	ClassAnnotation resetCache
]

{ #category : #running }
RegisteredClassAnnotationsTests >> tearDown [
		
	ClassAnnotation resetCache.
	
	super tearDown.
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testAreCached [

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.
	
	self assert: ClassAnnotationExample1 registeredInstances == annotations
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testAreDifferentForDifferentAnnotationClasses [

	| annotations1 annotations2 |
	annotations1 := ClassAnnotationExample1 registeredInstances.
	annotations2 := ClassAnnotationExample2 registeredInstances.
	
	self deny: annotations1 = annotations2
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testAreEmptyForUnusedAnnotation [

	| actual |
	actual := ClassAnnotationUnusedExample registeredInstances.
	
	self assert: actual isEmpty
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testAreSortedByPriority [

	| annotations expected |
	annotations := ClassAnnotationExampleWithPriority registeredInstances.
	
	self assert: annotations size equals: 3.
	
	expected := annotations sorted: #priority descending.
	self assert: annotations asArray equals: expected asArray
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testCanBeEnumerated [

	| enumerated |
	enumerated := OrderedCollection new.
	ClassAnnotationExample1 registeredInstancesDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassAnnotationExample1 registeredInstances asArray
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testCanBeQueriedForConcreteClass [

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstancesFor: ClassWithThreeAnnotations.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #annotatedClass as: Set) equals: {ClassWithThreeAnnotations} asSet
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testIncludeCopiesForEachSubclassOfOriginDeclaration [

	| annotations |
	annotations := ClassAnnotationExample3 registeredInstances.
	
	self assert: (annotations anySatisfy: [ :each | each annotatedClass = SubclassWithOnlyInheritedAnnotation ])
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testIncludeForbiddenClassesAsSpecialForbiddenAnnotations [
	| allForbidden forbidden actualAnnotation |
	
	allForbidden := ForbiddenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	forbidden := allForbidden detect: [:each | each declarationSelector = #forbiddenAnnotation].
	actualAnnotation := forbidden actualAnnotation.
	self assert: actualAnnotation class equals: ForbiddenClassAnnotationExample
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testIncludeInstancesFromAllAnnotatingMethodsOfAnyClassInSystem [

	| annotations |
	annotations := ClassAnnotationExample1 registeredInstances.
	
	self assert: annotations size equals: 2.
	self assert: (annotations collect: #class as: Set) equals: {ClassAnnotationExample1} asSet
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testNotIncludeForbiddenClassesAsBrokenAnnotations [
	| broken |
	
	broken := BrokenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	broken 
		detect: [:each | each declarationSelector = #forbiddenAnnotation]
		ifFound: [ :ann | self assert: false description: 'forbidden annotation should not be broken' ]
]

{ #category : #tests }
RegisteredClassAnnotationsTests >> testNotIncludeInstancesAnnotatingForbiddenClasses [
	"In that test the annotation define canAnnotateClass: with constant false answer.
	So any annotating methods which define it should never register their instances"
	| annotations |
	annotations := ForbiddenClassAnnotationExample registeredInstances.
	
	self assert: annotations isEmpty
]
