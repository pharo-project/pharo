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
AbstractDebugger class >> rankDebugSession: aDebugSession [
	"Returns an integer representing how much this debugger is suitable to debug @aDebugSession. The higher the integer returned, the more suitable this debugger is"
	self subclassResponsibility
]
