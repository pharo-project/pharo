"
Often one needs information from the base level at the meta level.

My classes model reifications of concept at the base level.
"
Class {
	#name : #RFReification,
	#superclass : #Object,
	#instVars : [
		'link',
		'entity'
	],
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFReification class >> allKeys [
	^self subclasses collect: [:plugin | plugin key]
]

{ #category : #'plugin interface' }
RFReification class >> entities [
	^self subclassResponsibility
]

{ #category : #'instance creation' }
RFReification class >> entity: aNodeOrVar link: aLink [ 
	^self new
		link: aLink;
		entity: aNodeOrVar.
]

{ #category : #'plugin interface' }
RFReification class >> for: aSelector [
	^ self subclasses detect: [ :class | class key = aSelector ]
]

{ #category : #testing }
RFReification class >> isUsed [
	"all my sublasses are used"
	^self name = 'RFReification'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #'plugin interface' }
RFReification class >> key [
	^self subclassResponsibility
]

{ #category : #acccessing }
RFReification >> entity: aRBMessageNode [ 
	entity := aRBMessageNode
]

{ #category : #generate }
RFReification >> genForClassVariable [
	^self genForLiteralVariable
]

{ #category : #generate }
RFReification >> genForGlobalVariable [
	^self genForLiteralVariable
]

{ #category : #generate }
RFReification >> genForInstanceVariableSlot [
	"overriden in subclass if needed"
]

{ #category : #generate }
RFReification >> genForLiteralVariable [
	"overriden in subclass if needed"
]

{ #category : #generate }
RFReification >> genForRBArgumentNode [
	^self genForRBTemporaryNode
]

{ #category : #generate }
RFReification >> genForRBAssignmentNode [
	^self genForRBProgramNode
]

{ #category : #generate }
RFReification >> genForRBBlockNode [
	^self genForRBProgramNode
]

{ #category : #generate }
RFReification >> genForRBCascadeNode [
	^self genForRBProgramNode 
]

{ #category : #generate }
RFReification >> genForRBGlobalNode [
	^self genForRBVariableNode 
]

{ #category : #generate }
RFReification >> genForRBInstanceVariableNode [
	^self genForRBVariableNode 
]

{ #category : #generate }
RFReification >> genForRBLiteralNode [
	^self genForRBProgramNode
]

{ #category : #generate }
RFReification >> genForRBLiteralValueNode [
	^self genForRBProgramNode 
]

{ #category : #generate }
RFReification >> genForRBMessageNode [
	^self genForRBProgramNode 
]

{ #category : #generate }
RFReification >> genForRBMethodNode [
	^self genForRBProgramNode 
]

{ #category : #generate }
RFReification >> genForRBProgramNode [
	"overriden in subclass if needed"
]

{ #category : #generate }
RFReification >> genForRBReturnNode [
	^self genForRBProgramNode
]

{ #category : #generate }
RFReification >> genForRBSelfNode [
	^self genForRBProgramNode 
]

{ #category : #generate }
RFReification >> genForRBSequenceNode [
	^self genForRBProgramNode
]

{ #category : #generate }
RFReification >> genForRBTemporaryNode [
	^self genForRBVariableNode
]

{ #category : #generate }
RFReification >> genForRBVariableNode [
	^self genForRBProgramNode
]

{ #category : #acccessing }
RFReification >> link: anUndefinedObject [ 
	link := anUndefinedObject
]

{ #category : #generate }
RFReification >> preamble: aNode [
	^#()
]
