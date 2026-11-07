"
I am a base class for commands which perform operations with particular ast-node of given method.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
	sourceNode:		<ASTProgramNode>
"
Class {
	#name : 'SycSourceCodeCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'method',
		'sourceNode'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycSourceCodeCommand class >> isAbstract [
	^self = SycSourceCodeCommand
]

{ #category : 'instance creation' }
SycSourceCodeCommand class >> method: aMethod node: aProgramNode [
	^self new
		method: aMethod;
		sourceNode: aProgramNode
]

{ #category : 'accessing' }
SycSourceCodeCommand >> method [
	^ method ifNil: [ self context ifNotNil: [ :ctx | ctx lastSelectedMethod ] ]
]

{ #category : 'accessing' }
SycSourceCodeCommand >> method: anObject [
	method := anObject
]

{ #category : 'accessing' }
SycSourceCodeCommand >> sourceNode [
	^ sourceNode ifNil: [ self context ifNotNil: [ :ctx | ctx lastSelectedSourceNode ] ]
]

{ #category : 'accessing' }
SycSourceCodeCommand >> sourceNode: anObject [
	sourceNode := anObject
]
