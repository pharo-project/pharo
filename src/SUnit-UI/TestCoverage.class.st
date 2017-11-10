"
I wrap a method in order to detect whether it has been called or not. Then I can be used to calculate test coverage for SUnit.
"
Class {
	#name : #TestCoverage,
	#superclass : #ProtoObject,
	#instVars : [
		'hasRun',
		'reference',
		'method'
	],
	#category : #SUnit-UI
}

{ #category : #'instance creation' }
TestCoverage class >> on: aMethodReference [
	^ self new initializeOn: aMethodReference
]

{ #category : #private }
TestCoverage >> doesNotUnderstand: aMessage [
	^ method perform: aMessage selector withArguments: aMessage arguments
]

{ #category : #testing }
TestCoverage >> hasRun [
	^ hasRun
]

{ #category : #initialization }
TestCoverage >> initializeOn: aMethodReference [
	hasRun := false.
	reference := aMethodReference.
	method := reference compiledMethod
]

{ #category : #actions }
TestCoverage >> install [
	reference actualClass methodDict
		at: reference selector
		put: self
]

{ #category : #private }
TestCoverage >> mark [
	hasRun := true
]

{ #category : #private }
TestCoverage >> reference [
	^ reference
]

{ #category : #evaluation }
TestCoverage >> run: aSelector with: anArray in: aReceiver [
	self mark; uninstall.
	^ aReceiver withArgs: anArray executeMethod: method
]

{ #category : #actions }
TestCoverage >> uninstall [
	reference actualClass methodDict
		at: reference selector
		put: method
]
