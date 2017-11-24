"
An ExceptionSet is a grouping of exception handlers which acts as a single handler.  Within the group, the most recently added handler will be the last handler found during a handler search (in the case where more than one handler in the group is capable of handling a given exception). 
"
Class {
	#name : #ExceptionSet,
	#superclass : #Object,
	#instVars : [
		'exceptions'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptionselector }
ExceptionSet >> , anException [
	"Return an exception set that contains the receiver and the argument exception. This is commonly used to specify a set of exception selectors for an exception handler."

	self add: anException
]

{ #category : #exceptionselector }
ExceptionSet >> - anotherException [
	"Create an exception set containnig the receiver
	and anotherException as an exclusion."

	^ ExceptionSetWithExclusions new
		add: self;
		addExclusion: anotherException;
		yourself
]

{ #category : #private }
ExceptionSet >> add: anException [
	"Add anException to the exceptions that I handle"
	
	^ exceptions add: anException
]

{ #category : #exceptionselector }
ExceptionSet >> handles: anException [
	"Determine whether an exception handler will accept a signaled exception."

	^ exceptions anySatisfy: [ :exception | exception handles: anException ]
]

{ #category : #initialization }
ExceptionSet >> initialize [

	super initialize.
	exceptions := OrderedCollection new
]
