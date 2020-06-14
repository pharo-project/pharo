"
A RenameCommand is command that renames selected node with newSource.

"
Class {
	#name : #RenameCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #execute }
RenameCommand >> execute [
	| newNode |
	newNode := self panel parse: newSource.
	self changeCommandReceiverFor: newNode.
	^ newNode
]

{ #category : #accessing }
RenameCommand >> length [
	^ self newSource size - 1
]
