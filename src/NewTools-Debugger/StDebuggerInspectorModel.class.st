"
I am a model representing the first variable lists in the debugger inspector.

"
Class {
	#name : #StDebuggerInspectorModel,
	#superclass : #StInspectorModel,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerInspectorModel >> allInspectorNodes [

	^ self inspectedObject buildInspectorNodes
]

{ #category : #accessing }
StDebuggerInspectorModel >> debuggerContextClass [
	^ StDebuggerContext
]

{ #category : #accessing }
StDebuggerInspectorModel >> defaultObjectInspectorClass [

	^ StDebuggerRawObjectInspector
]

{ #category : #initialize }
StDebuggerInspectorModel >> initialize [
	super initialize.
	inspectedObject := self debuggerContextClass new
]

{ #category : #accessing }
StDebuggerInspectorModel >> inspectedObject [

	^ inspectedObject ifNil: [ inspectedObject := self debuggerContextClass new ]
]

{ #category : #accessing }
StDebuggerInspectorModel >> labelString [
	^super labelString capitalized
]
