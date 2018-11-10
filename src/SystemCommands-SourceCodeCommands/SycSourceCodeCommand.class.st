"
I am a base class for commands which perform operations with particular ast-node of given method.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
	sourceNode:		<RBProgramNode>
"
Class {
	#name : #SycSourceCodeCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'method',
		'sourceNode'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #'instance creation' }
SycSourceCodeCommand class >> method: aMethod node: aRBProgramNode [
	^self new 
		method: aMethod;
		sourceNode: aRBProgramNode
]

{ #category : #accessing }
SycSourceCodeCommand >> method [
	^ method
]

{ #category : #accessing }
SycSourceCodeCommand >> method: anObject [
	method := anObject
]

{ #category : #execution }
SycSourceCodeCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	
	method := aSourceCodeContext lastSelectedMethod.
	sourceNode := aSourceCodeContext lastSelectedSourceNode
]

{ #category : #accessing }
SycSourceCodeCommand >> sourceNode [
	^ sourceNode
]

{ #category : #accessing }
SycSourceCodeCommand >> sourceNode: anObject [
	sourceNode := anObject
]
