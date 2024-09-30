Class {
	#name : #RBCompositeEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		#otherEnvironment
	],
	#category : #'Refactoring-Environment'
}

{ #category : #accessing }
RBCompositeEnvironment >> description [
	^ (environment descriptionUntil: 15),
		self operator,
		(otherEnvironment  descriptionUntil: 15)
]

{ #category : #testing }
RBCompositeEnvironment >> isCompositeEnvironment [
	^ true
]

{ #category : #accessing }
RBCompositeEnvironment >> operator [
	self subclassResponsibility
]

{ #category : #accessing }
RBCompositeEnvironment >> otherEnvironment [
	^ otherEnvironment
]

{ #category : #accessing }
RBCompositeEnvironment >> shortDescription [
	^ self description
]
