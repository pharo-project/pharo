"
I am the root of the github markup tree
"
Class {
	#name : #MicRootBlock,
	#superclass : #MicAbstractBlock,
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicRootBlock >> accept: aVisitor [
 	^ aVisitor visitRoot: self
]

{ #category : #testing }
MicRootBlock >> addLineAndReturnNextNode: line [
	"add line to this node. 
	Notice, the action is allowed to create new nodes in the block tree.
	Returns the node to handle next line - typically self."

	| newBlock |
	newBlock := self newBlockFor: line parent: self.
	^ newBlock 
]

{ #category : #testing }
MicRootBlock >> canConsumeLine: line [
	"The root block can always consume everything no matter what it is."

	^ true
]

{ #category : #accessing }
MicRootBlock >> indent [
	^0
]
