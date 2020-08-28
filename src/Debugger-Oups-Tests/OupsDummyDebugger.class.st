"
I am a dummy debugger.
My instances simulate debugger classes that are named, and can handle any context.
"
Class {
	#name : #OupsDummyDebugger,
	#superclass : #Object,
	#instVars : [
		'name',
		'tag',
		'deferred',
		'usable',
		'error'
	],
	#category : #'Debugger-Oups-Tests-Utils'
}

{ #category : #helpers }
OupsDummyDebugger class >> dummyDebuggerList [
	^ OrderedCollection
		withAll:
			{(self named: #A).
			(self named: #B).
			(self named: #C)}
]

{ #category : #helpers }
OupsDummyDebugger class >> dummySession [
	| context process session |
	context := [ Set new ] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	session := process
		newDebugSessionNamed: 'test session'
		startedAt: context.
	session exception: (OupsNullException fromSignallerContext: context).
	^ session
]

{ #category : #'instance creation' }
OupsDummyDebugger class >> named: aString [
	^self new name: aString
]

{ #category : #accessing }
OupsDummyDebugger >> beError [
	error := true
]

{ #category : #accessing }
OupsDummyDebugger >> beUnusable [
	usable := false
]

{ #category : #'instance creation' }
OupsDummyDebugger >> debugSession: aDebugSession [
	self isError
		ifFalse: [ tag := aDebugSession.
			^ self ].
	tag := Error new.
	tag signalIn: thisContext.
	tag signal
]

{ #category : #'deferred message' }
OupsDummyDebugger >> defer: aBlock [
	aBlock value.
	deferred := true 
]

{ #category : #accessing }
OupsDummyDebugger >> deferred [
	^ deferred ifNil:[deferred := false]
]

{ #category : #helpers }
OupsDummyDebugger >> findContextSuchThat: aBlock [
	^self
]

{ #category : #testing }
OupsDummyDebugger >> handlesContext: aContext [
	^self isUsable 
]

{ #category : #accessing }
OupsDummyDebugger >> isError [
	^ error ifNil: [ error := false ]
]

{ #category : #accessing }
OupsDummyDebugger >> isUsable [
	^ usable ifNil: [ usable := true ]
]

{ #category : #helpers }
OupsDummyDebugger >> methodToDebug [
	| i |
	i := 0.
	10 timesRepeat: [ i := i + 1 ].
	^ i
]

{ #category : #accessing }
OupsDummyDebugger >> name [
	^ name
]

{ #category : #accessing }
OupsDummyDebugger >> name: anObject [
	name := anObject
]

{ #category : #accessing }
OupsDummyDebugger >> receiver [
	^self
]

{ #category : #debugging }
OupsDummyDebugger >> signalDebuggerError: exception [
	(OupsDebuggerFailure of: self on: exception) signal
]

{ #category : #accessing }
OupsDummyDebugger >> tag [
	^ tag
]

{ #category : #accessing }
OupsDummyDebugger >> tag: anObject [
	tag := anObject
]

{ #category : #'ui requests' }
OupsDummyDebugger >> warningDefaultAction: aWarningException [
	tag := aWarningException
]

{ #category : #helpers }
OupsDummyDebugger >> zeroDivide [
	1/0
]
