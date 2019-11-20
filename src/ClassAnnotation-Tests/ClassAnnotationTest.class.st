Class {
	#name : #ClassAnnotationTest,
	#superclass : #ClassAnnotationTestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #running }
ClassAnnotationTest >> tearDown [
	"Some tests redefine following annotation. 
	Here we clear redefining state which forces cache reset"
	ClassAnnotationExample3 revertRedefinedInstances.
	"ClassAnnotationExample3 = ClassWithSingleAnnotation classAnnotations anyOne class"
	
	super tearDown.
]

{ #category : #tests }
ClassAnnotationTest >> testCheckIfAnnotatesGivenClass [
	| ann|
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann annotates: ClassWithSingleAnnotation).
	self deny: (ann annotates: ClassWithThreeAnnotations)
]

{ #category : #tests }
ClassAnnotationTest >> testComparisonOfTwoAnnotationsWithDifferentClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann1 declarationSelector: #selector1.

	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	ann2 declarationSelector: #selector1.

	self deny: ann1 equals: ann2
]

{ #category : #tests }
ClassAnnotationTest >> testComparisonOfTwoAnnotationsWithDifferentSelector [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.

	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector2.

	self deny: ann1 equals: ann2
]

{ #category : #tests }
ClassAnnotationTest >> testComparisonOfTwoAnnotationsWithSameSelectorAndClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann1 declarationSelector: #selector1.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithThreeAnnotations.
	ann2 declarationSelector: #selector1.
	
	self assert: ann1 equals: ann2.
	self assert: ann1 hash equals: ann2 hash
]

{ #category : #tests }
ClassAnnotationTest >> testComparisonOfTwoDifferentAnnotations [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann1 declarationSelector: #selector1.

	ann2 := ClassAnnotationExample2 withAnnotatedClass: ClassWithSingleAnnotation.
	ann2 declarationSelector: #selector1.

	self deny: ann1 equals: ann2
]

{ #category : #tests }
ClassAnnotationTest >> testCreationForExplicitAnnotationContext [
	| ann context |
	context := SimpleAnnotationContext of: AnnotationUserExample.
	ann := ClassAnnotationExample1 for: context.
	
	self assert: ann class equals: ClassAnnotationExample1.
	self assert: ann activeContext equals: context.
]

{ #category : #tests }
ClassAnnotationTest >> testForgettingAnnotation [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: ClassWithSingleAnnotation classAnnotations isEmpty
]

{ #category : #tests }
ClassAnnotationTest >> testGettingAllRedefinedInstances [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefined := annotation class redefinedInstances.	
	self assert: allRedefined size equals: 1.
	self assert: allRedefined anyOne priority equals: 0
]

{ #category : #tests }
ClassAnnotationTest >> testGettingAllRedefinedInstancesShouldCleanGarbage [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	allRedefined := annotation class redefinedInstances.	
	self assert: allRedefined isEmpty
]

{ #category : #tests }
ClassAnnotationTest >> testGettingAllRedefiningInstances [
	| annotation allRedefining |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	allRedefining := annotation class redefiningInstances.
	self assert: allRedefining size equals: 1.
	self assert: allRedefining anyOne priority equals: -1000.
	self assert: allRedefining anyOne identicalTo: annotation
]

{ #category : #tests }
ClassAnnotationTest >> testGettingAllRedefiningInstancesShouldCleanGarbage [
	| annotation allRedefined |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	allRedefined := annotation class redefiningInstances.	
	self assert: allRedefined isEmpty
]

{ #category : #tests }
ClassAnnotationTest >> testGettingFreshInstance [
	| ann actual |
	ann := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	ann declarationSelector: #annotationExample. 
	
	actual := ann freshInstance.
	
	self assert: actual equals: ClassWithSingleAnnotation annotationExample
]

{ #category : #tests }
ClassAnnotationTest >> testGettingRedefinedInstance [
	| annotation redefinedInstance |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	redefinedInstance := annotation redefinedInstance.
	self deny: redefinedInstance identicalTo: annotation.
	self assert: redefinedInstance priority equals: 0.
	self assert: redefinedInstance isRedefined
]

{ #category : #tests }
ClassAnnotationTest >> testGettingRedefiningInstance [
	| annotation actual |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].

	actual := annotation copy redefiningInstance.

	self assert: actual identicalTo: annotation
]

{ #category : #tests }
ClassAnnotationTest >> testGettingSingleAnnotationUsingSelector [
	| expected actual |
	expected := ClassWithThreeAnnotations classAnnotations detect: [ :each | each declarationSelector = #annotationExample2 ].
	actual := ClassWithThreeAnnotations classAnnotationAt: #annotationExample2.

	self assert: actual identicalTo: expected
]

{ #category : #tests }
ClassAnnotationTest >> testHasNullContextByDefault [
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann activeContext equals: NullAnnotationContext instance
]

{ #category : #tests }
ClassAnnotationTest >> testHasZeroPriorityByDefault [
	
	| ann |
	ann := ClassAnnotationExample1 new.
	
	self assert: ann priority equals: 0
]

{ #category : #tests }
ClassAnnotationTest >> testNotSimilarToAnnotationOfDifferentClass [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: SubclassWithOnlyInheritedAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTest >> testNotSimilarToAnnotationOfSameClassButForDifferentContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTest >> testNotSimilarToDifferentKindAnnotation [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample2 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self deny: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTest >> testObsolete [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self deny: annotation isObsolete.
	
	ClassAnnotation registry forgetAnnotation: annotation.
	
	self assert: annotation isObsolete
]

{ #category : #tests }
ClassAnnotationTest >> testRedefiningInstance [
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
ClassAnnotationTest >> testRedefiningInstanceTwice [
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
ClassAnnotationTest >> testRedefiningInstanceUsingBlockWithArgument [
	| newAnnotation |
	
	ClassWithSingleAnnotation classAnnotations anyOne
		redefineBy: [:annotation | annotation priority: -1000 ].
		
	newAnnotation := ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: newAnnotation priority equals: -1000.
	self assert: newAnnotation isRedefined
]

{ #category : #tests }
ClassAnnotationTest >> testRevertingAllRedefinedInstances [
	| annotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation class revertRedefinedInstances.
	
	self assert: annotation class redefinedInstances isEmpty.
	self assert: annotation class redefiningInstances isEmpty
]

{ #category : #tests }
ClassAnnotationTest >> testRevertingRedefinedInstance [
	| annotation revertedAnnotation |
	annotation := ClassWithSingleAnnotation classAnnotations anyOne.
	annotation redefineBy: [ annotation priority: -1000 ].
	revertedAnnotation := annotation revertRedefinedInstance.

	self deny: revertedAnnotation identicalTo: annotation.
	self assert: revertedAnnotation identicalTo: ClassWithSingleAnnotation classAnnotations anyOne.
	self assert: revertedAnnotation priority equals: 0.
	self deny: revertedAnnotation isRedefined
]

{ #category : #tests }
ClassAnnotationTest >> testSimilarToAnnotationOfSameClassForSameContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 for: AnnotationUserExample withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)
]

{ #category : #tests }
ClassAnnotationTest >> testSimilarToAnnotationOfSameClassWhenBothDoNotDefinedWithContext [
	| ann1 ann2 |
	ann1 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	ann2 := ClassAnnotationExample1 withAnnotatedClass: ClassWithSingleAnnotation.
	
	self assert: (ann1 isSimilarTo: ann2)
]
