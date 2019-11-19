Class {
	#name : #CompositeAnnotationContextTest,
	#superclass : #TestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #tests }
CompositeAnnotationContextTest >> testComparison [
	self assert: Object asAnnotationContext , Array asAnnotationContext equals: Object asAnnotationContext , Array asAnnotationContext.
	self assert: (Object asAnnotationContext , Array asAnnotationContext) hash equals: (Object asAnnotationContext , Array asAnnotationContext) hash.

	self deny: Object asAnnotationContext , Array asAnnotationContext equals: Object asAnnotationContext , String asAnnotationContext
]

{ #category : #tests }
CompositeAnnotationContextTest >> testCreationByComma [
	
	| context1 context2 context |
	context1 := Object asAnnotationContext.
	context2 := Array asAnnotationContext.
	
	context := context1 , context2.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. context2}.
]

{ #category : #tests }
CompositeAnnotationContextTest >> testCreationByCommaWithCompatibleArgument [
	
	| context1 context |
	context1 := Object asAnnotationContext.
	
	context := context1 , Array.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. Array asAnnotationContext}.
]

{ #category : #tests }
CompositeAnnotationContextTest >> testCreationByCommaWithMultipleCompatipleContexts [
	
	| context1 context |
	context1 := Object asAnnotationContext.
	
	context := context1 , Array, String.
	
	self assert: context class equals: CompositeAnnotationContext.
	self 
		assert: context parts 
		equals: {context1. Array asAnnotationContext. String asAnnotationContext}
]

{ #category : #tests }
CompositeAnnotationContextTest >> testCreationByCommaWithMultipleContexts [
	
	| context1 context2 context context3 |
	context1 := Object asAnnotationContext.
	context2 := Array asAnnotationContext.
	context3 := String asAnnotationContext.
	
	context := context1 , context2, context3.
	
	self assert: context class equals: CompositeAnnotationContext.
	self assert: context parts equals: {context1. context2. context3}.
]
