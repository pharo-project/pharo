Class {
	#name : #ClassAnnotationTests,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #running }
ClassAnnotationTests >> tearDown [
	"Some tests redefine following annotation. 
	Here we clear redefining state which forces cache reset"
	ClassAnnotationExample3 revertRedefinedInstances.
	"ClassAnnotationExample3 = ClassWithSingleAnnotation classAnnotations anyOne class"
	
	super tearDown.
]

{ #category : #tests }
ClassAnnotationTests >> testCheckIfAnnotatesGivenClass [
	| ann|
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann annotates: ClassWithSingleAnnotation).
	self deny: (ann annotates: ClassWithThreeAnnotations)
]

{ #category : #tests }
ClassAnnotationTests >> testComparisonOfTwoAnnotationsWithDifferentClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	ann2 declarationSelector: #selector1.
	
	self deny: ann1 = ann2
]

{ #category : #tests }
ClassAnnotationTests >> testComparisonOfTwoAnnotationsWithDifferentSelector [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector2.
	
	self deny: ann1 = ann2
]

{ #category : #tests }
ClassAnnotationTests >> testComparisonOfTwoAnnotationsWithSameSelectorAndClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector1.
	
	self assert: ann1 equals: ann2.
	self assert: ann1 hash equals: ann2 hash
]

{ #category : #tests }
ClassAnnotationTests >> testComparisonOfTwoDifferentAnnotations [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample2 withAnnotatedClass: ClassWithSingleAnnotation.
	ann2 declarationSelector: #selector1.
	
	self deny: ann1 = ann2
]

{ #category : #tests }
ClassAnnotationTests >> testCreationForExplicitAnnotationContext [
	| ann context |
	context := SimpleAnnotationContext of: AnnotationUserExample.
	ann := ClassAnnotationExample1 for: context.
	
	self assert: ann class equals: ClassAnnotationExample1.
	self assert: ann activeContext equals: context.
]

{ #category : #tests }
ClassAnnotationTests >> testForgettingAnnotation [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: ClassWithSingleAnnotation classAnnotations isEmpty
]

{ #category : #tests }
ClassAnnotationTests >> testGettingAllRedefinedInstances [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefined := annotation class redefinedInstances.	
	self assert: allRedefined size equals: 1.
	self assert: allRedefined anyOne priority equals: 0
]

{ #category : #tests }
ClassAnnotationTests >> testGettingAllRedefinedInstancesShouldCleanGarbage [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	allRedefined := annotation class redefinedInstances.	
	self assert: allRedefined isEmpty
]

{ #category : #tests }
ClassAnnotationTests >> testGettingAllRedefiningInstances [
	| annotation allRedefining |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefining := annotation class redefiningInstances.	
	self assert: allRedefining size equals: 1.
	self assert: allRedefining anyOne priority equals: -1000.
	self assert: allRedefining anyOne == annotation
]

{ #category : #tests }
ClassAnnotationTests >> testGettingAllRedefiningInstancesShouldCleanGarbage [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	allRedefined := annotation class redefiningInstances.	
	self assert: allRedefined isEmpty
]

{ #category : #tests }
ClassAnnotationTests >> testGettingFreshInstance [
	| ann actual |
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann declarationSelector: #annotationExample. 
	
	actual := ann freshInstance.
	
	self assert: actual equals: ClassWithSingleAnnotation annotationExample
]

{ #category : #tests }
ClassAnnotationTests >> testGettingRedefinedInstance [
	| annotation redefinedInstance |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	redefinedInstance := annotation redefinedInstance.
	self deny: redefinedInstance == annotation.
	self assert: redefinedInstance priority equals: 0.
	self assert: redefinedInstance isRedefined
]

{ #category : #tests }
ClassAnnotationTests >> testGettingRedefiningInstance [
	| annotation actual |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	actual := annotation copy redefiningInstance.
	
	self assert: actual == annotation
]

{ #category : #tests }
ClassAnnotationTests >> testGettingSingleAnnotationUsingSelector [
	| expected actual |
	
	expected := ClassWithThreeAnnotations classAnnotations 
		detect: [ :each | each declarationSelector = #annotationExample2 ].
	actual := ClassWithThreeAnnotations classAnnotationAt: #annotationExample2.
	
	self assert: actual == expected
]

{ #category : #tests }
ClassAnnotationTests >> testHasNullContextByDefault [
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann activeContext equals: NullAnnotationContext instance
]

{ #category : #tests }
ClassAnnotationTests >> testHasZeroPriorityByDefault [
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann priority equals: 0
]

{ #category : #tests }
ClassAnnotationTests >> testNotSimilarToAnnotationOfDifferentClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTests >> testNotSimilarToAnnotationOfSameClassButForDifferentContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTests >> testNotSimilarToDifferentKindAnnotation [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample2 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTests >> testObsolete [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self deny: annotation isObsolete.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: annotation isObsolete
]

{ #category : #tests }
ClassAnnotationTests >> testRedefiningInstance [
	| annotation newAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	annotation redefineBy: [ annotation priority: -1000 ].
	self assert: annotation priority equals: -1000.
	self assert: annotation isRedefined.
	
	ClassAnnotation resetCache.
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -1000.
	self assert: newAnnotation isRedefined
]

{ #category : #tests }
ClassAnnotationTests >> testRedefiningInstanceTwice [
	| annotation newAnnotation reverted |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	annotation redefineBy: [ annotation priority: -1000 ].
	self assert: annotation redefinedInstance priority equals: 0.
	annotation redefineBy: [ annotation priority: -2000 ].
	self assert: annotation redefinedInstance priority equals: 0.
	
	ClassAnnotation resetCache.
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -2000.
	newAnnotation revertRedefinedInstance.
	reverted := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: reverted priority equals: 0.
]

{ #category : #tests }
ClassAnnotationTests >> testRedefiningInstanceUsingBlockWithArgument [
	| newAnnotation |
	
	ClassWithSingleAnnotation classAnnotations anyOne
		redefineBy: [:annotation | annotation priority: -1000 ].
		
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -1000.
	self assert: newAnnotation isRedefined
]

{ #category : #tests }
ClassAnnotationTests >> testRevertingAllRedefinedInstances [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation class revertRedefinedInstances.
	
	self assert: annotation class redefinedInstances isEmpty.
	self assert: annotation class redefiningInstances isEmpty
]

{ #category : #tests }
ClassAnnotationTests >> testRevertingRedefinedInstance [
	| annotation revertedAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	revertedAnnotation := annotation revertRedefinedInstance.

	self deny: revertedAnnotation == annotation.	
	self assert: revertedAnnotation == ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: revertedAnnotation priority equals: 0.
	self deny: revertedAnnotation isRedefined
]

{ #category : #tests }
ClassAnnotationTests >> testSimilarToAnnotationOfSameClassForSameContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTests >> testSimilarToAnnotationOfSameClassWhenBothDoNotDefinedWithContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)
]
