"
I am an abstract class to define the minimal API of the Debugger.

My subclasses can be registred like this:

	NoDebugger beCurrent
"
Class {
	#name : #AbstractDebugger,
	#superclass : #Object,
	#category : #'Tools-Debugger'
}

{ #category : #query }
AbstractDebugger class >> beCurrent [
	self isAbstract ifTrue: [ self error: 'Cannot register an abstract debugger' ].

	Smalltalk tools register: self as: #debugger
]

{ #category : #testing }
AbstractDebugger class >> isAbstract [
	^ self = AbstractDebugger
]

{ #category : #'opening api' }
AbstractDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self subclassResponsibility
]
