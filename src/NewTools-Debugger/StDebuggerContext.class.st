"
I am a simple wrapper holding the current context and the current debug session in a debugger.
My purpose is only to display context and debug session related information in the debugger inspector.
"
Class {
	#name : #StDebuggerContext,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'instance creation' }
StDebuggerContext class >> context: aContext [

	^ self new
		  context: aContext;
		  yourself
]

{ #category : #nodes }
StDebuggerContext >> argumentsNodes [

	| argNames |
	argNames := self context sourceNode arguments.
	^ (argNames collect: [ :argName | 
		   (StDebuggerInspectorTempNode hostObject: self context) 
			   argVariable: (self context temporaryVariableNamed: argName name) ])
		  asOrderedCollection
]

{ #category : #accessing }
StDebuggerContext >> buildInspectorNodes [

	| nodes argsNodes argsKeys tempsNodes |
	nodes := OrderedCollection new.
	argsNodes := self argumentsNodes.
	argsKeys := argsNodes collect: [ :node | node key ].
	tempsNodes := self temporaryVariablesNodes reject: [ :tempNode | 
		              argsKeys includes: tempNode key ].
	nodes addAll: argsNodes.
	nodes addAll: tempsNodes.
	nodes addAll: self receiverNodes.
	nodes add: self stackTopNode.
	nodes add: self thisContextNode.
	^ nodes
]

{ #category : #accessing }
StDebuggerContext >> context [

	^ context ifNil: [ context := [] asContext ]
]

{ #category : #accessing }
StDebuggerContext >> context: anObject [
	context := anObject
]

{ #category : #printing }
StDebuggerContext >> printOn: aStream [
	aStream nextPutAll: context asString 
]

{ #category : #nodes }
StDebuggerContext >> receiverNodes [

	^ self context receiver allInspectorNodes reject: [ :node | node key = 'self' ]
]

{ #category : #nodes }
StDebuggerContext >> stackTopNode [
	^StInspectorStackTopNode hostObject: self context
]

{ #category : #nodes }
StDebuggerContext >> temporaryVariablesNodes [

	^ self context astScope allTemps collect: [ :temp | 
		  (StDebuggerInspectorTempNode hostObject: self context) tempVariable: temp ]
]

{ #category : #nodes }
StDebuggerContext >> thisContextNode [ 
	^StInspectorThisContextNode hostObject: self context
]
