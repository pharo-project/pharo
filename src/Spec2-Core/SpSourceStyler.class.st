Class {
	#name : #SpSourceStyler,
	#superclass : #SpTextStyler,
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #'as yet unclassified' }
SpSourceStyler >> applyStyleTo: aString inContext: anObject [

	self prepareWithinContext: anObject.
	^ self applyStyleTo: aString
]

{ #category : #'as yet unclassified' }
SpSourceStyler >> prepareWithinContext: anObject [
	"Style of source code may happen in a specific context (code context). 
	 For example, in Pharo is not the same to style as a method than as a playground (or in 
	 other execution context). 
	 Override this method to allow the styler to take context before applying a style."
]
