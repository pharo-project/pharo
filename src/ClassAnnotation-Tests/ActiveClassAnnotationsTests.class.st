Class {
	#name : #ActiveClassAnnotationsTests,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
ActiveClassAnnotationsTests >> testCanBeEnumeratedForGivenClassAndUser [

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
ActiveClassAnnotationsTests >> testCanBeEnumeratedForGivenUser [

	| annotationUser actual expected |
	annotationUser := AnnotationUserExample new.
	actual := OrderedCollection new.
	ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser do: [:each | 
		actual add: each].
	expected := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.

	self assert: actual asSet equals: expected asSet.

]

{ #category : #tests }
ActiveClassAnnotationsTests >> testIncludeOnlyInstancesWhichAreActiveForGivenUser [

	| actual annotationUser expected |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: true. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	expected := ContextualClassAnnotationExample1 visibleInstancesInContext: annotationUser.	
	self assert: actual asSet equals: expected asSet
]

{ #category : #tests }
ActiveClassAnnotationsTests >> testNotIncludeInstancesWhichAreNotActiveForGivenUser [

	| actual annotationUser |
	annotationUser := AnnotationUserExample new.
	annotationUser isSimulatedActiveAnnotation: false. 
	
	actual := ContextualClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: actual isEmpty
]

{ #category : #tests }
ActiveClassAnnotationsTests >> testNotIncludeInstancesWithoutContext [

	| annotations annotationUser |
	annotationUser := AnnotationUserExample new.
	annotations := ClassAnnotationExample1 activeInstancesInContext: annotationUser.
	
	self assert: annotations isEmpty
]
