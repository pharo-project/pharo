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

	| context |
	context := self hostObject context.
	^ context tempNames collect: [ :name | 
		  (StDebuggerInspectorTempNode hostObject: context) tempVariable:
			  (context temporaryVariableNamed: name) ]
]

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> label [
	^ 'temps'" asText makeAllColor: self colorForTempNode"
]

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> rawValue [
	| context variables mapping |
	context := self hostObject context.
	variables := context temporaryVariables.
	mapping := Dictionary new.
	variables do: [ :tempVariable | 
		mapping
			at: tempVariable name
			put: (tempVariable readInContext: context) ].
	^ mapping
]

{ #category : #accessing }
StDebuggerContextInspectorTempsNode >> stringValue [
	^''
]
