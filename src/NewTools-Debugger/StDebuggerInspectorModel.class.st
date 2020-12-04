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
StDebuggerInspectorModel >> defaultObjectInspectorClass [

	^ StDebuggerRawObjectInspector
]

{ #category : #initialize }
StDebuggerInspectorModel >> initialize [
	super initialize.
	inspectedObject := StDebuggerContext new
]

{ #category : #accessing }
StDebuggerInspectorModel >> inspectedObject [

	^ inspectedObject ifNil: [ inspectedObject := StDebuggerContext new ]
]

{ #category : #accessing }
StDebuggerInspectorModel >> labelString [
	^super labelString capitalized
]
