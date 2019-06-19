Class {
	#name : #QueryAnnotationsFromClassTests,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
QueryAnnotationsFromClassTests >> testAllowDebugBrokenAnnotations [

	| annotations ann |
	annotations := ClassWithBrokenAnnotatingMethod classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	self assert: ann class equals: BrokenClassAnnotation.

	[ann debug.
	self assert: false description: 'should fail'] 
		on: Error do: [ :err | 
	self assert: err messageText equals: ClassWithBrokenAnnotatingMethod brokenAnnotationMessage  ]
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testAllowDebugForbiddenBrokenAnnotations [

	| annotations ann |
	annotations := ClassWithForbiddenAnnotationExample classAnnotations.	
	self assert: annotations size equals: 2. "onw forbidden and one broken"
	
	ann := (annotations reject: #isForbidden) anyOne.
	self assert: ann class equals: BrokenClassAnnotation.

	[ann debug.
	self assert: false description: 'should fail'] 
		on: Error do: [ :err |  ]
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testCanBeEnumerated [

	| enumerated |
	enumerated := OrderedCollection new.
	ClassWithThreeAnnotations classAnnotationsDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassWithThreeAnnotations classAnnotations asArray
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testReturnsInstancesFromAllAnnotatingMethods [

	| annotations |
	annotations := ClassWithThreeAnnotations classAnnotations.
	
	self assert: annotations size equals: 3.
	annotations do: [ :each | 
		self assert: each annotatedClass equals: ClassWithThreeAnnotations]
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenAnnotationFailsCheckForForbiddenProperty [

	| annotations ann |
	[ ClassAnnotationExampleWithFailedForbiddenCheck new isForbidden.
	self assert: false description: 'should fail'  ] ifError: [ ].

	annotations := ClassWithForbiddenAnnotationExample classAnnotations.	
	self assert: annotations size equals: 2. "one forbidden and one broken"
	
	ann := (annotations reject: #isForbidden) anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithForbiddenAnnotationExample.
	self assert: ann declarationSelector equals: #failedForbiddenAnnotation

]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenClassHasNoAnnotations [

	| annotations |
	annotations := ClassWithoutAnnotations classAnnotations.
	
	self assert: annotations isEmpty
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenDeclaringMethodIsBroken [

	| annotations ann |
	annotations := ClassWithBrokenAnnotatingMethod classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithBrokenAnnotatingMethod.
	self assert: ann declarationSelector equals: #brokenAnnotation.
	self assert: (ann errorString endsWith: ClassWithBrokenAnnotatingMethod brokenAnnotationMessage).
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenDeclaringMethodNotReturnAnnotation [

	| annotations ann |
	annotations := ClassWithBadAnnotatingMethod classAnnotations.	
	self assert: annotations size equals: 1.
	
	ann := annotations anyOne.
	
	self assert: ann class equals: BrokenClassAnnotation.
	self assert: ann annotatedClass equals: ClassWithBadAnnotatingMethod.
	self assert: ann declarationSelector equals: #badAnnotation
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenItOverridesSuperclassAnnotations [

	| annotations |
	annotations := SubclassWithOverridenAnnotation classAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations anyOne annotatedClass equals: SubclassWithOverridenAnnotation.
	self assert: annotations anyOne class equals: ClassAnnotationExample2.
]

{ #category : #tests }
QueryAnnotationsFromClassTests >> testWhenSuperclassIsAnnotatedToo [

	| annotations |
	annotations := SubclassWithOnlyInheritedAnnotation classAnnotations.
	
	self assert: annotations size equals: 1.
	self assert: annotations anyOne annotatedClass equals: SubclassWithOnlyInheritedAnnotation
]
