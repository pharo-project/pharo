"
I am a model representing the first variable lists in the debugger inspector.

"
Class {
	#name : #StDebuggerInspectorModel,
	#superclass : #StInspectorModel,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerInspectorModel >> defaultObjectInspectorClass [
	^StDebuggerRawObjectInspector
]

{ #category : #accessing }
StDebuggerInspectorModel >> labelString [
	^ 'Receiver in: ' , super labelString
]
