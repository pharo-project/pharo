"
I am a plain text paragraph.

If am divided from other paragraphs by either a blank line, or one of the other markups.
"
Class {
	#name : #MicParagraphBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'text'
	],
	#category : #'Microdown-Model'
}

{ #category : #'instance creation' }
MicParagraphBlock class >> opensBy: aLine [
	^ aLine notEmpty 
]

{ #category : #parsing }
MicParagraphBlock >> addLineAndReturnNextNode: line [
	"add line to this node. 
	Notice, the action is allowed to create new nodes in the block tree.
	Returns the node to handle next line - typically self."

	text := text
		ifNil: [ line trim ]
		ifNotNil: [ text , String cr , line trim ].
	^ self
]

{ #category : #private }
MicParagraphBlock >> asUnindented: line [
	"return a line where the indentation to my indentation level has been removed"

	^ line copyFrom: self indent + 1 to: line size
]

{ #category : #parsing }
MicParagraphBlock >> canConsumeLine: line [
	"A paragraph is closed by an empty line, "

	| unIndented |
	line ifEmpty: [ ^ false ].
	(self isRightlyIndented: line)
		ifFalse: [ ^ false ].
	unIndented := self asUnindented: line.
	^ (self blockStarterClassFrom: unIndented) = self class
]

{ #category : #public }
MicParagraphBlock >> contents [ 
	^super contents , '[ ', self text, ' ]'
]

{ #category : #private }
MicParagraphBlock >> isRightlyIndented: line [
	"test if line has the appropriate number of spaces at first"

	self indent = 0
		ifTrue: [ ^ true ].
	^ line beginsWith: (' ' repeat: self indent)
]

{ #category : #printing }
MicParagraphBlock >> printOn: aStream [

	aStream nextPutAll: 'Paragraph: '.
	aStream nextPutAll: (self text asString contractTo: 100)
]

{ #category : #accessing }
MicParagraphBlock >> text [
	^ text
]
