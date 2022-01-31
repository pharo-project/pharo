"
An environment is delimited by a start and end markup and it can have an argument list. 
By default I do not handle any form of formatting in the potential arguments.

## Syntax


```
<?

?>
```


This class needs another pass to support nested mic elements inside its delimiter.

In Pillar
```
${begin:column|width=40}$
blah blah
${end:column}$
```

```
${begin:card}$

# I'm Cool

A paragraph about the card.
I will be interpreted as part of the card.

${end:card}$ 
```





This is important to support better column in the future:

```
${slide:title=Class Methods}$

${columns}$

${column:width=60}$

+>file://figures/Point-xy-class-method.png|width=90+

${column:width=40}$

- press the button ==class== to define a class method
- in lectures, we add ==class==

${endColumns}$
```


### Implementation limits
The contents of an environment cannot really contain other blocks.

"
Class {
	#name : #MicEnvironmentBlock,
	#superclass : #MicStartStopMarkupBlock,
	#instVars : [
		'arguments',
		'name'
	],
	#category : #'Microdown-Model'
}

{ #category : #public }
MicEnvironmentBlock class >> alternateBlockClassFor: line [
	"If there is one subclass with the corresponding tag, returns it, else resturn the current class."
	"line is of the form <?slide|title=Schedule"
	
	"We know that the two first characters are <? else we would be invoked"
	| tag |
	tag := ((line allButFirst: 2) copyUpTo: $|) trimBoth.
	^ self allSubclasses detect: [ :each | each tag = tag ] ifNone: [ self ]
	
	
	
]

{ #category : #visiting }
MicEnvironmentBlock >> accept: aVisitor [
 	^ aVisitor visitEnvironment: self
]

{ #category : #adding }
MicEnvironmentBlock >> addLineAndReturnNextNode: line [
	"add line to this node. 
	Notice, the action is allowed to create new nodes in the block tree.
	Returns the node to handle next line - typically self."
	
	isClosed
		ifTrue: [ ^ self ].
	(self doesLineStartWithStopMarkup: line)
		ifTrue: [ ^ self ].
	firstLine
		ifNil:
			[ firstLine := self extractFirstLineFrom: line ]
		ifNotNil: [ ^ self bodyFromLine: line ].
	^ self
]

{ #category : #accessing }
MicEnvironmentBlock >> arguments [
	
	^ arguments
]

{ #category : #'parse support' }
MicEnvironmentBlock >> body [

	^ String streamContents: [:s |  self bodyElements do: [ :each | s nextPutAll: each substring ] ]
]

{ #category : #'parse support' }
MicEnvironmentBlock >> bodyElements [

	^ children
]

{ #category : #'parse support' }
MicEnvironmentBlock >> bodyFromLine: line [
	
	| newBlock |
	newBlock := (self newBlockFor: line parent: self).
	self parser setCurrent: newBlock.
	^ newBlock
]

{ #category : #'parse support' }
MicEnvironmentBlock >> closeMe [

	"this is a temporary solution because normally the parser should populate body with MicBlocks.
	Right now just make sure that we have structured text = inline elements. "
	"body ifNotNil: [ 
		body :=  self inlineParse: body ].
	children := body"
]

{ #category : #accessing }
MicEnvironmentBlock >> extractFirstLineFrom: aLine [

	| lineWithoutMarkup argumentsSplitter |
	lineWithoutMarkup := super extractFirstLineFrom: aLine.
	argumentsSplitter := MicArgumentsSplitter withString: lineWithoutMarkup.
	name := argumentsSplitter name trimBoth.
	arguments := argumentsSplitter arguments.
	^ lineWithoutMarkup
]

{ #category : #markups }
MicEnvironmentBlock >> lineStartMarkup [

	^ parser environmentOpeningBlockMarkup 
]

{ #category : #markups }
MicEnvironmentBlock >> lineStopMarkup [
	
	^ parser environmentClosingBlockMarkup 
]

{ #category : #accessing }
MicEnvironmentBlock >> name [

	^ name
]
