"
I am an abstract class for the two kind of lists, ordered and unordered.
When I created I automatically add a listitem as my child.
Unordered list line starts with `-`,  ordered list starts with number.
An empty or a wrong indent closes me.

Here is an example:
- bullet list item 1
- bullet list item 2
  1. sublist ordered item 2.1
  3. sublist ordered item 2.2
- bullet list item 3

Indentation (two spaces) matters and is crucial. 
Here is an example
```
- item 1
xx- subitem 1.1
xx- subitem 2.1
- item 2
xx- subitem 2.1
xxxx- subsubitem 2.1.1
xx- subitem 2.2
```
produces:
- item 1
  - subitem 1.1
  - subitem 2.1
- item 2
  - subitem 2.1
    - subsubitem 2.1.1
  - subitem 2.2

Now an ordered list can also contain an unordered list.
1. first
2. second 
   - subitem 2.1
   - subitem 2.2
3. third



"
Class {
	#name : #MicListBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'indent'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicListBlock class >> isAbstract [

	^ self == MicListBlock
]

{ #category : #testing }
MicListBlock >> addLineAndReturnNextNode: inLine [
	"Create a new item, and insert it into me.
	line is on the format 'markup text'"

	| line |
	inLine ifEmpty: [ ^ self ].
	"A list accept an empty - like that it avoids to close the block (the list) and we can have space between list item.
	Now we do nothing with empty so just do not create new item or whatever."
	line := inLine trim.
	self indentFromLine: line.
	^ self listItemBlockClass new
		setParser: parser; 
		parent: self;
		addLineAndReturnNextNode: (line copyFrom: indent - parent indent to: line size)
]

{ #category : #testing }
MicListBlock >> canConsumeLine: line [
	"to consume this line there must be a ListItem starting at the right indentation"
	
	(parent indent = 0
		or: [ line beginsWith: (' ' repeat: parent indent) ])
		ifFalse: [ ^ false ].
	^true
]

{ #category : #accessing }
MicListBlock >> indent [
	^ indent
]

{ #category : #indent }
MicListBlock >> indentFromLine: aLine [
	"The line is expected to have the form: '<bullet> space+ [text]', where bullet is one of *,+,- or 1. or 1). More than one space is allowed. 
	indentFromLine returns the position of the last character before the text.
	The indentation returned is *the number of characters*, that is, tab counts for but one char."

	| regEx |
	indent ifNotNil: [ ^self ].
	regEx := '(\S+)(\s*)' asRegex.
	regEx matchesPrefix: aLine.
	indent := parent indent + (regEx subexpression: 1) size
]
