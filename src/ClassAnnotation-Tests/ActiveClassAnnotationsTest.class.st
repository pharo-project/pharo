Class {
	#name : #ActiveClassAnnotationsTest,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
ActiveClassAnnotationsTest >> testCanBeEnumeratedForGivenClassAndUser [

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 
		activeInstancesFor: ClassWithAnnotationsDefinedForUserClass inContext: annotationUser 
		do: [:each | actual add: each].
	expected := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet
]

{ #category : #tests }
ActiveClassAnnotationsTest >> testCanBeEnumeratedForGivenUser [

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser do: [:each | 
		actual add: each].
	expected := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet.

]

{ #category : #tests }
ActiveClassAnnotationsTest >> testIncludeOnlyInstancesWhichAreActiveForGivenUser [

	| actual annotationUser expected |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: true. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.	
	self assert: actual asSet equals: expected asSet
]

{ #category : #tests }
ActiveClassAnnotationsTest >> testNotIncludeInstancesWhichAreNotActiveForGivenUser [

	| actual annotationUser |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: false. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: actual isEmpty
]

{ #category : #tests }
ActiveClassAnnotationsTest >> testNotIncludeInstancesWithoutContext [

	| annotations annotationUser |
	annotationUser := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty
]
