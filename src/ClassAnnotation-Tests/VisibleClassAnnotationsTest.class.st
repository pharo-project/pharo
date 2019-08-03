Class {
	#name : #VisibleClassAnnotationsTest,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
VisibleClassAnnotationsTest >> testCanBeEnumeratedForGivenClassAndUser [

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 
		visibleInstancesFor: ClassWithAnnotationsDefinedForUserClass inContext: annotationUser 
		do: [:each | actual add: each].
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet
]

{ #category : #tests }
VisibleClassAnnotationsTest >> testCanBeEnumeratedForGivenUser [

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser do: [:each | 
		actual add: each].
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet.

]

{ #category : #tests }
VisibleClassAnnotationsTest >> testIncludeInstancesDeclaredForSuperclassOfGivenUser [

	| annotations annotationUser declaredContext |
	annotationUser := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample2 visibleInstancesInContext: annotationUser.
	
	self assert: annotations size equals: 1.
	declaredContext := annotations anyOne activeContext annotationUserClass.
	self assert: declaredContext equals: BaseAnnotationUserExample
]

{ #category : #tests }
VisibleClassAnnotationsTest >> testIncludeOnlyInstancesCreatedForGivenUser [

	| annotations annotationUser declaredContexts |
	annotationUser := AnnotationUserExample new.
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations size equals: 2.
	declaredContexts := annotations collect: [:each | each activeContext annotationUserClass].
	self assert: declaredContexts asSet equals: {AnnotationUserExample} asSet
]

{ #category : #tests }
VisibleClassAnnotationsTest >> testNotIncludeInstancesWithoutContext [

	| annotations annotationUser |
	annotationUser := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty
]

{ #category : #tests }
VisibleClassAnnotationsTest >> testWhenThereIsNoOne [

	| annotations annotationUser |
	annotationUser := Object new. "context can be anything"
	
	annotations := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty
]
