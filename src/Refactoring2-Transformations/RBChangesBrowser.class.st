"
Deprecated!!! Use superclass
"
Class {
	#name : #RBChangesBrowser,
	#superclass : #ChangesBrowser,
	#instVars : [
		'whenAcceptBlock'
	],
	#category : #'Refactoring2-Transformations-Utilities'
}

{ #category : #deprecation }
RBChangesBrowser class >> deprecated [
	^ true
]

{ #category : #deprecation }
RBChangesBrowser class >> new [
	self deprecated: 'Use superclass ' on: '4 May 2016' in: #Pharo60.
	^ super new
]

{ #category : #deprecation }
RBChangesBrowser class >> systemIcon [
	^ Smalltalk ui icons iconNamed: #packageDelete
]

{ #category : #api }
RBChangesBrowser >> accept [

	super accept.
	whenAcceptBlock value.
]

{ #category : #accessing }
RBChangesBrowser >> whenAcceptBlock: anObject [
	whenAcceptBlock := anObject
]
