Class {
	#name : #StDebuggerContextInspectorArgumentNode,
	#superclass : #StDebuggerContextInspectorNode,
	#instVars : [
		'argumentName'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorArgumentNode >> argumentName: anObject [
	argumentName := anObject
]

{ #category : #accessing }
StDebuggerContextInspectorArgumentNode >> computeChildrenFromSourceObject [
	^ self value allInspectorNodes reject: [ :node | node key = 'self' ]
]

{ #category : #accessing }
StDebuggerContextInspectorArgumentNode >> label [
	^('[arg] ' , argumentName) "asText makeAllColor: self colorForArgNode"
]

{ #category : #accessing }
StDebuggerContextInspectorArgumentNode >> rawValue [
	^ hostObject
]

{ #category : #accessing }
StDebuggerContextInspectorArgumentNode >> stringValue [
	^self rawValue printString "asText makeAllColor: self colorForArgNode"
]
