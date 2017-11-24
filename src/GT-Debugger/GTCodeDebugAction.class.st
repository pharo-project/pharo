"
A GTCodeDebugAction encapsulates the logic for accessing the codePresentation from a debugger.

"
Class {
	#name : #GTCodeDebugAction,
	#superclass : #DebugAction,
	#instVars : [
		'codePresentationSelector'
	],
	#category : #'GT-Debugger-Actions'
}

{ #category : #accessing }
GTCodeDebugAction >> codePresentation [
	^ self debugger perform: self codePresentationSelector
]

{ #category : #accessing }
GTCodeDebugAction >> codePresentationSelector [
	^ codePresentationSelector
]

{ #category : #accessing }
GTCodeDebugAction >> codePresentationSelector: aSelector [
	codePresentationSelector := aSelector
]

{ #category : #initialization }
GTCodeDebugAction >> initialize [
	super initialize.
	self codePresentationSelector: #codePresentation
]
