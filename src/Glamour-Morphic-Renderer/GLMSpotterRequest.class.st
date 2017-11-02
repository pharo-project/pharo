Class {
	#name : #GLMSpotterRequest,
	#superclass : #Notification,
	#instVars : [
		'requests',
		'prompt',
		'default'
	],
	#category : 'Glamour-Morphic-Renderer-Spotter'
}

{ #category : #initialize-release }
GLMSpotterRequest >> add: aRequest [
	requests add: aRequest 
]

{ #category : #accessing }
GLMSpotterRequest >> default [
	^ default
]

{ #category : #accessing }
GLMSpotterRequest >> default: anObject [
	default := anObject
]

{ #category : #initialize-release }
GLMSpotterRequest >> defaultAction [
	^(GLMMorphicSpotter openOn: self) answer
]

{ #category : #initialize-release }
GLMSpotterRequest >> handleWith: anObject [
	^ anObject handleCompletionRequest: self
]

{ #category : #initialize-release }
GLMSpotterRequest >> initialize [
	super initialize.
	requests := OrderedCollection new.
]

{ #category : #accessing }
GLMSpotterRequest >> prompt [
	^ prompt
]

{ #category : #accessing }
GLMSpotterRequest >> prompt: anObject [
	prompt := anObject
]

{ #category : #accessing }
GLMSpotterRequest >> requests [
	^requests 
]
