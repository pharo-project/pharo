"
I represent a node in the debugger inspector list, with special computation for retrieving my children and my key (= displayed label).
"
Class {
	#name : #StDebuggerContextInspectorNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'textStyler'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorNode >> children [ 
	^self computeChildrenFromSourceObject
]

{ #category : #styling }
StDebuggerContextInspectorNode >> colorForArgNode [
	^(self textStyler attributesFor: #symbol) first asColor
	

]

{ #category : #styling }
StDebuggerContextInspectorNode >> colorForSelfNode [
	^(self textStyler attributesFor: #self) first asColor
]

{ #category : #styling }
StDebuggerContextInspectorNode >> colorForTempNode [
	^(self textStyler attributesFor: #tempVar) first asColor
]

{ #category : #accessing }
StDebuggerContextInspectorNode >> computeChildrenFromSourceObject [
	^self subclassResponsibility 
]

{ #category : #accessing }
StDebuggerContextInspectorNode >> key [
	^self label
]

{ #category : #accessing }
StDebuggerContextInspectorNode >> textStyler [
	^textStyler ifNil:[textStyler := SHRBTextStyler new ]
]
