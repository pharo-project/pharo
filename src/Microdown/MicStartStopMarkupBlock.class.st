"
May be in the future we will be able to say that SameMarkup is the same as a StartStop with the same delimiter but for now we do not know. 
"
Class {
	#name : #MicStartStopMarkupBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'isClosed',
		'body',
		'firstLine'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicStartStopMarkupBlock class >> isAbstract [

	^ self == MicStartStopMarkupBlock
]

{ #category : #handle }
MicStartStopMarkupBlock >> addLineAndReturnNextNode: line [
	"add line to this node. 
	Notice, the action is allowed to create new nodes in the block tree.
	Returns the node to handle next line - typically self."

	isClosed
		ifTrue: [ ^ self ].
	firstLine
		ifNil:
			[ firstLine := self extractFirstLineFrom: line ]
		ifNotNil: [ self bodyFromLine: line ].
	^ self
]

{ #category : #accessing }
MicStartStopMarkupBlock >> body [
	^ body
]

{ #category : #accessing }
MicStartStopMarkupBlock >> body: anObject [
	^ body := anObject
]

{ #category : #handle }
MicStartStopMarkupBlock >> bodyFromLine: line [
	self flag: #todo. 
	"I do not think that this is a good idea to use a string to represent lines.
	Especially since line contents is not interpreted - no formatting"
	
	body := body ifNil: [ line ] ifNotNil: [ body , String cr , line ]
]

{ #category : #testing }
MicStartStopMarkupBlock >> canConsumeLine: line [
	"As soon as a line closes a code block, it does not accept anymore any line.
	Indeed imagine 
	
	<
	a line 
	>
	The first line was accepted by its parent (a root block and the code block got created.
	then a line was accepting since done was false...
	then the closing line was reached and the code block got closed.)
	
	"

	isClosed
		ifTrue: [ ^ false ].
	isClosed := self doesLineStartWithStopMarkup: line.
	^ true
]

{ #category : #testing }
MicStartStopMarkupBlock >> doesLineStartWithMarkup: line [
	"return if the line starts with a markup"

	^ line beginsWith: self lineStartMarkup
	
]

{ #category : #testing }
MicStartStopMarkupBlock >> doesLineStartWithStopMarkup: line [
	"return if the line starts with a stop markup"

	^ line beginsWith: self lineStopMarkup
	
]

{ #category : #handle }
MicStartStopMarkupBlock >> extractFirstLineFrom: line [
	^ line copyFrom: self lineStartMarkup size + 1 to: line size
]

{ #category : #accessing }
MicStartStopMarkupBlock >> firstLine [
	^ firstLine
]

{ #category : #accessing }
MicStartStopMarkupBlock >> firstLine: anObject [
	firstLine := anObject
]

{ #category : #initialization }
MicStartStopMarkupBlock >> initialize [
	super initialize. 
	isClosed := false
]

{ #category : #testing }
MicStartStopMarkupBlock >> isClosed [
	^ isClosed		
]

{ #category : #markups }
MicStartStopMarkupBlock >> lineStartMarkup [

	^ self subclassResponsibility 
]

{ #category : #markups }
MicStartStopMarkupBlock >> lineStopMarkup [

	^ self subclassResponsibility 
]
