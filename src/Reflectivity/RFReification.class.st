"
Often one needs information from the base level at the meta level.

My classes model reifications of concept at the base level.
"
Class {
	#name : 'RFReification',
	#superclass : 'Object',
	#instVars : [
		'link',
		'entity'
	],
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFReification class >> allKeys [
	^self subclasses collect: [:plugin | plugin key]
]

{ #category : 'plugin interface' }
RFReification class >> entities [
	"Returns the places (i.e. node classes) where I can be used"
	^self subclassResponsibility
]

{ #category : 'instance creation' }
RFReification class >> entity: aNodeOrVar link: aLink [
	^self new
		link: aLink;
		entity: aNodeOrVar
]

{ #category : 'plugin interface' }
RFReification class >> for: aSelector [
	^ self subclasses detect: [ :class | class key = aSelector ]
]

{ #category : 'testing' }
RFReification class >> isUsed [
	"all my sublasses are used"
	^self name = 'RFReification'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : 'plugin interface' }
RFReification class >> key [
	"Returns my 'name'"
	^self subclassResponsibility
]

{ #category : 'accessing' }
RFReification >> entity: aMessageNode [
	entity := aMessageNode
]

{ #category : 'generate' }
RFReification >> genForASTArrayNode [

	^ self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTAssignmentNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTBlockNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTCascadeNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTLiteralArrayNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTLiteralNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTLiteralValueNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTMessageNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTMethodNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTPragmaNode [
	^ self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTProgramNode [
	"overriden in subclass if needed"
]

{ #category : 'generate' }
RFReification >> genForASTReturnNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTSequenceNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForASTVariableNode [
	^self genForASTProgramNode
]

{ #category : 'generate' }
RFReification >> genForClassVariable [
	^self genForLiteralVariable
]

{ #category : 'generate' }
RFReification >> genForGlobalVariable [
	^self genForLiteralVariable
]

{ #category : 'generate' }
RFReification >> genForInstanceVariableSlot [
	"overriden in subclass if needed"
]

{ #category : 'generate' }
RFReification >> genForLiteralVariable [
	"overriden in subclass if needed"
]

{ #category : 'accessing' }
RFReification >> link: anUndefinedObject [
	link := anUndefinedObject
]

{ #category : 'generate' }
RFReification >> postamble: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preamble: aNode [
	(aNode isKindOf: OCProgramNode) ifFalse: [ ^#() ]. "variables not yet suported"
	aNode isMessage ifTrue: [ ^self preambleForMessage: aNode ].
	aNode isMethod	ifTrue: [ ^self preambleForMethod: aNode ].
	aNode isSequence ifTrue: [ ^self preambleSequence: aNode ].
	aNode isReturn ifTrue: [ ^ self preambleForReturn: aNode ].
	aNode isDynamicArray ifTrue: [ ^self preambleForArray: aNode ].
	aNode isAssignment ifTrue: [ ^self preambleForAssignment: aNode ].
	aNode isVariable ifTrue: [ ^self preambleForVariable: aNode ].
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForArray: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForAssignment: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForMessage: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForMethod: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForReturn: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleForVariable: aNode [
	^#()
]

{ #category : 'preamble' }
RFReification >> preambleSequence: aNode [
	^#()
]
