"
stepRecord: StepRecord

StepRecords: LinkedList[StepRecord]
"
Class {
	#name : #SindarinDebugSession,
	#superclass : #Object,
	#instVars : [
		'triggerEventOn',
		'canBeTerminated',
		'debugSession'
	],
	#category : #'Sindarin-Base'
}

{ #category : #'instance creation' }
SindarinDebugSession class >> newWithName: aString forProcess: aProcess [

	^ DebugSession new
		  name: aString;
		  process: aProcess context: aProcess suspendedContext;
		  asSindarinDebugSession
]

{ #category : #initialization }
SindarinDebugSession >> activateEventTriggering [
	triggerEventOn := true.
	self flag: 'Why not refreshing?'.
	"self refreshAttachedDebugger."
]

{ #category : #converting }
SindarinDebugSession >> asSindarinDebugSession [

	^ self
]

{ #category : #accessing }
SindarinDebugSession >> canBeTerminated [

	^ canBeTerminated
]

{ #category : #accessing }
SindarinDebugSession >> canBeTerminated: anObject [

	canBeTerminated := anObject
]

{ #category : #initialization }
SindarinDebugSession >> deactivateEventTriggering [

	triggerEventOn := false
]

{ #category : #accessing }
SindarinDebugSession >> debugSession [

	^ debugSession
]

{ #category : #accessing }
SindarinDebugSession >> debugSession: anObject [

	debugSession := anObject
]

{ #category : #initialization }
SindarinDebugSession >> initialize [

	super initialize.
	triggerEventOn := true.
	canBeTerminated := true
]

{ #category : #initialization }
SindarinDebugSession >> refreshAttachedDebugger [
	"The following lines are to force possible debuggers observing the same debug session to update themselves based"

	self debugSession triggerEvent: #contextChanged
]

{ #category : #'debugging actions' }
SindarinDebugSession >> resumeAndClear [

	self debugSession
		resume;
		clear
]

{ #category : #'debugging actions' }
SindarinDebugSession >> stepInto: aContext [
	"Should not step more a process that is terminating, otherwise the image will get locked."
	self flag: 'Why the image gets locked? Please investigate.'.

	self debugSession interruptedProcess isTerminating
		ifTrue: [ SteppingATerminatingProcess signal ].
	^ self debugSession stepInto: aContext
]

{ #category : #'debugging actions' }
SindarinDebugSession >> stepOver: aContext [
	"Should not step more a process that is terminating, otherwise the image will get locked."
	self flag: 'Why the image gets locked? Please investigate.'.
	
	self debugSession interruptedProcess isTerminating
		ifTrue: [ SteppingATerminatingProcess signal ].
	^ self debugSession stepOver: aContext
]

{ #category : #'debugging actions' }
SindarinDebugSession >> terminate [

	canBeTerminated ifTrue: [ ^ self debugSession terminate ]
]

{ #category : #'debugging actions' }
SindarinDebugSession >> triggerEvent: anEventSelector [

	triggerEventOn ifTrue: [ 
		^ self debugSession triggerEvent: anEventSelector ]
]
