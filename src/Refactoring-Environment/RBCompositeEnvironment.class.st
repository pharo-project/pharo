Class {
	#name : 'RBCompositeEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#instVars : [
		'otherEnvironment'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'description' }
RBCompositeEnvironment >> description [

	^ label ifNil: [
		  (environment descriptionUntil: 15) , self operator
		  , (otherEnvironment descriptionUntil: 15) ]
]

{ #category : 'testing' }
RBCompositeEnvironment >> isCompositeEnvironment [
	^ true
]

{ #category : 'accessing' }
RBCompositeEnvironment >> operator [
	self subclassResponsibility
]

{ #category : 'accessing' }
RBCompositeEnvironment >> otherEnvironment [
	^ otherEnvironment
]

{ #category : 'removing' }
RBCompositeEnvironment >> removeAll [
	environment removeAll.
	otherEnvironment removeAll
]

{ #category : 'accessing' }
RBCompositeEnvironment >> shortDescription [
	^ self description
]
