"
I represent one item in a list (ordered or non-ordered)
"
Class {
	#name : #MicListItemBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'text'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicListItemBlock >> addLineAndReturnNextNode: line [
	"add line to this node. 
	Notice, the action is allowed to create new nodes in the block tree.
	Returns the node to handle next line - typically self."
	
	
	| normalized |
	text
		ifNil: [ text := line trim.
			^ self ].
	normalized := line copyFrom: parent indent + 1 to: line size.	"drop indent"
	"SD: I thought that a list item was holding a paragraph but it holds directly text."
		
	(self isAList: normalized)
		ifTrue: [ ^ self newBlockFor: normalized parent: self ]
		ifFalse: [ text := text , String cr , normalized ]
]

{ #category : #testing }
MicListItemBlock >> canConsumeLine: line [
	"return if this block can consume line"

	^ line beginsWith: (' ' repeat: parent indent)
]

{ #category : #debugging }
MicListItemBlock >> contents [ 
	^super contents , '[ ', self text, ' ]'
]

{ #category : #testing }
MicListItemBlock >> isAList: normalized [

	^ parser isAList: normalized
]

{ #category : #debugging }
MicListItemBlock >> printOn: aStream [

	aStream nextPutAll: 'List Item: '.
	aStream nextPutAll: (self text asString contractTo: 100)
]

{ #category : #accessing }
MicListItemBlock >> text [
	^ text
]
