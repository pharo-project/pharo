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

{ #category : #visiting }
MicListItemBlock >> accept: aVisitor [
 	^ aVisitor visitListItem: self
]

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

{ #category : #visiting }
MicListItemBlock >> closeMe [
	super closeMe.
	text := self inlineParse: text.
	
]

{ #category : #accessing }
MicListItemBlock >> computeNestedLevel [
	"An item has the same nested level than its parent."
	^ self parent computeNestedLevel
]

{ #category : #testing }
MicListItemBlock >> isAList: normalized [

	^ parser isAList: normalized
]

{ #category : #accessing }
MicListItemBlock >> nestedLevel [
	"Return the nesting level of main blocks. Basically only list increases this."
	
	^ self parent nestedLevel 
]

{ #category : #accessing }
MicListItemBlock >> text [
	"Returns a string we should rename text into textString"
	^ String streamContents: [ :s | self textElements do: [:each | s nextPutAll: each substring ]]
]

{ #category : #accessing }
MicListItemBlock >> textElements [ 
	
	^ text 
]
