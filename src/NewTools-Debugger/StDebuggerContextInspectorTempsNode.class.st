"
I retrieve receiver nodes from a StDebuggerContext object.
My children are extracted from a StDebugger context indexed slots.
"
Class {
	#name : #StDebuggerContextInspectorTempsNode,
	#superclass : #StDebuggerContextInspectorNode,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> computeChildrenFromSourceObject [
	^ (StNodeCollector for: hostObject) collectTempsNodes
]

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> label [
	^ 'Temps'
]

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> rawValue [
	^ hostObject context temporaries
]
