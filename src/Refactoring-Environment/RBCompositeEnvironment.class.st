Class {
	#name : 'RBCompositeEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#instVars : [
		'otherEnvironment'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'comparing' }
RBCompositeEnvironment >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ environment = anObject environment and: [otherEnvironment = anObject otherEnvironment]
]

{ #category : 'accessing' }
RBCompositeEnvironment >> definedClasses [
	| definedClasses |
	definedClasses := IdentitySet new: 4096.
	self definedClassesDo: [ :each | each isMeta ifFalse: [definedClasses add: each] ].
	^ definedClasses reject: [ :each | each isMeta ]
]

{ #category : 'description' }
RBCompositeEnvironment >> description [

	^ label ifNil: [
		  (environment descriptionUntil: 15) , self operator
		  , (otherEnvironment descriptionUntil: 15) ]
]

{ #category : 'comparing' }
RBCompositeEnvironment >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ otherEnvironment hash
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
