"
This is the abstract class for the Glamour loggers. These classes are typically used for debugging hte highly dynamic Glamour model.
"
Class {
	#name : #GLMLogger,
	#superclass : #Object,
	#classInstVars : [
		'nullInstance',
		'instance'
	],
	#category : #'Glamour-Helpers'
}

{ #category : #accessing }
GLMLogger class >> instance [
	^ instance ifNil: [
		instance := self nullInstance ]
]

{ #category : #accessing }
GLMLogger class >> instance: aLogger [
	instance := aLogger 
]

{ #category : #accessing }
GLMLogger class >> nullInstance [ 
	^ nullInstance ifNil: [
		nullInstance := GLMNullLogger new ]
]

{ #category : #initialization }
GLMLogger class >> reset [
	instance := nil
]

{ #category : #logging }
GLMLogger >> logAnnouncement: anAnnouncement from: aGLMObject [ 
	
	
]
