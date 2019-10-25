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
AbstractDebugger class >> handlesContext: aContext [
	"Returns whether this debugger is suitable to debug @aContext"
	self subclassResponsibility 
]

{ #category : #'debugger declaration' }
AbstractDebugger class >> iAmADebugger [
	"Leave blank for intermediate classes. Add the <iAmADebugger> pragma for concrete debugger classes. This pragma is used by the system to find all available debuggers"
	self subclassResponsibility 
]

{ #category : #testing }
AbstractDebugger class >> isAbstract [
	^ self = AbstractDebugger
]

{ #category : #'opening api' }
AbstractDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	self subclassResponsibility
]

{ #category : #'debugger declaration' }
AbstractDebugger class >> rank [
	"Returns an int. When an exception needs to  be debugged, the rank is used to choose among the available debuggers. A higher rank means a higher priority."
	self subclassResponsibility
]
