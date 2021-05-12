Class {
	#name : #RBCompositeEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'otherEnvironment'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #testing }
RBCompositeEnvironment >> isCompositeEnvironment [
	^ true
]

{ #category : #accessing }
RBCompositeEnvironment >> otherEnvironment [ 
	^ otherEnvironment
]
