"
I am a simple wrapper holding the current context and the current debug session in a debugger.
My purpose is only to display context and debug session related information in the debugger inspector.
"
Class {
	#name : #StDebuggerContext,
	#superclass : #Object,
	#instVars : [
		'context',
		'session',
		'assertionContext',
		'exceptionContext',
		'debugger'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'instance creation' }
StDebuggerContext class >> context: aContext session: aDebugSession debugger: sourceDebugger [
	^ self new
		context: aContext;
		session: aDebugSession;
		debugger: sourceDebugger;
		yourself
]

{ #category : #accessing }
StDebuggerContext >> allInspectorNodes [
	^ self argumentsNodes
		  addAll: { 
				  (StDebuggerContextInspectorReceiverNode hostObject: self).
				  (StDebuggerContextInspectorTempsNode hostObject: self).
				  (StInspectorStackTopNode hostObject: self context).
				  (StDebuggerContextInspectorContextNode hostObject: self) };
		  yourself
]

{ #category : #accessing }
StDebuggerContext >> argumentsNodes [
	| argNames |
	argNames := self context sourceNode arguments.
	^ (argNames collect: [ :argName | 
		   (StDebuggerInspectorTempNode hostObject: self context) argVariable:
			   (self context temporaryVariableNamed: argName name) ])
		  asOrderedCollection
]

{ #category : #accessing }
StDebuggerContext >> assertionSelectors [
	self flag: 'A bit magickal, also is this the right place?'.
	^ #( #assert:equals: #assert:closeTo:precision:
	     #assert:closeTo: #assert:identicalTo: #assertCollection:equals:
	     #assertCollection:hasSameElements: )
]

{ #category : #accessing }
StDebuggerContext >> context [
	^ context
]

{ #category : #accessing }
StDebuggerContext >> context: anObject [
	context := anObject
]

{ #category : #accessing }
StDebuggerContext >> debugger: anObject [
	debugger := anObject
]

{ #category : #printing }
StDebuggerContext >> printOn: aStream [
	aStream nextPutAll: context asString 
]

{ #category : #accessing }
StDebuggerContext >> session [
	^ session
]

{ #category : #accessing }
StDebuggerContext >> session: anObject [
	session := anObject
]
