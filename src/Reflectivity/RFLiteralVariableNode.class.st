"
I am a global binding. 
"
Class {
	#name : #RFLiteralVariableNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'binding'
	],
	#category : #Reflectivity-Compiler
}

{ #category : #'instance creation' }
RFLiteralVariableNode class >> value: anObject [
	^self new binding: #RFMetaLink -> anObject
]

{ #category : #visiting }
RFLiteralVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralVariableNode:  self
]

{ #category : #accessing }
RFLiteralVariableNode >> binding [ 
	^binding
]

{ #category : #accessing }
RFLiteralVariableNode >> binding: anAssocation [
	binding := anAssocation
]

{ #category : #testing }
RFLiteralVariableNode >> isFaulty [
	^false
]

{ #category : #accessing }
RFLiteralVariableNode >> start [
	^0
]

{ #category : #accessing }
RFLiteralVariableNode >> stop [
	^1
]
