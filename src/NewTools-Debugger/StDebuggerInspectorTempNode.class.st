"
Special node to display context temprs and arguments in the debugger inspector
"
Class {
	#name : #StDebuggerInspectorTempNode,
	#superclass : #StInspectorTempNode,
	#instVars : [
		'label'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerInspectorTempNode >> argVariable: aVariable [

	super tempVariable: aVariable.
	label := '[arg] ' , tempVariable name
]

{ #category : #accessing }
StDebuggerInspectorTempNode >> children [
	^ super children reject: [ :node | node key = 'self' ]
]

{ #category : #accessing }
StDebuggerInspectorTempNode >> computeChildrenFromSourceObject [
	^ self value allInspectorNodes reject: [ :node | node key = 'self' ]
]

{ #category : #accessing }
StDebuggerInspectorTempNode >> label [
	^label
]

{ #category : #accessing }
StDebuggerInspectorTempNode >> tempVariable: aVariable [

	super tempVariable: aVariable.
	label := tempVariable name
]
