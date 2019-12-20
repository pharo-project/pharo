Class {
	#name : #SimpleAnnotationContextTest,
	#superclass : #TestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
SimpleAnnotationContextTest >> testComparison [
	self assert: Object asAnnotationContext equals: Object asAnnotationContext.
	self assert: Object asAnnotationContext hash equals: Object asAnnotationContext hash.

	self deny: Object asAnnotationContext equals: Array asAnnotationContext
]
