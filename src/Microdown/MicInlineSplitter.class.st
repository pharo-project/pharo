"
I represent the class which is in charge of splitting sentences to parse inline blocks.

To proceed: `MicInlineSplitter new start: stringToParse`

### Implementation: How parsing is made ? 
- Read the string to parse and memorize all delimiters we meet in a collection
- Start the `emphasisProcess`
   - Search for the first opener delimiter in collection
   - If it’s a link or a figure opener delimiter, start the `linkOrFigureProcess`
     - Search for the associate closer
     - Search for the following url
     - Add it to the final array of inlines blocks
   - Search for the associate closer
   - Check if we have nested inline blocks (like bold in italic format)
   - Add the found inline block to the final array of inlines blocks

Note1: the escape delimiter is `\` (back slash)
Note2: a new line in an inline paragraph doesn’t interfere with parsing. 
The following example is totally bold:
```
		**Hello
		world**
```
Note3: BUT a line between two lines must end the parsing process.
The following example must not be bold:

```
		**Hello

		world**
```
"
Class {
	#name : #MicInlineSplitter,
	#superclass : #Object,
	#instVars : [
		'string',
		'resultArray',
		'index',
		'delimiterStack',
		'allDelimiters',
		'incrementation'
	],
	#category : #'Microdown-Parser'
}

{ #category : #'class factory' }
MicInlineSplitter >> abstractDelimiterClass [
	^ MicAbstractDelimiter
]

{ #category : #accessing }
MicInlineSplitter >> allDelimiters [
	^ allDelimiters
]

{ #category : #process }
MicInlineSplitter >> delimiterFoundProcess: aDelimiterSubclass [
	(aDelimiterSubclass type = #escape) 
		ifTrue: [ incrementation := incrementation + 1 ]
		ifFalse: [ delimiterStack add: (aDelimiterSubclass index: index) ]
]

{ #category : #'class factory' }
MicInlineSplitter >> emphasisProcessClass [
	^ MicInlineEmphasisProcessor
]

{ #category : #accessing }
MicInlineSplitter >> index [
	^ index
]

{ #category : #actions }
MicInlineSplitter >> indexIncrement [
	^ self indexIncrement: 1
]

{ #category : #actions }
MicInlineSplitter >> indexIncrement: anInteger [
	index := self index + anInteger
]

{ #category : #initialization }
MicInlineSplitter >> initialize [
	super initialize.
	self initializeDelimiters
]

{ #category : #initialization }
MicInlineSplitter >> initializeDelimiters [
	allDelimiters := Dictionary new.
	self abstractDelimiterClass subclasses 
		select: [ :subclass | subclass isActive ]
		thenDo: [ :subclass | allDelimiters 
										at: subclass markup 
										put: subclass ]
]

{ #category : #process }
MicInlineSplitter >> linkOrImageProcess [
	| linkNameCloserIndex |
	"1. search the linkname closer delimiter"
	linkNameCloserIndex := delimiterStack findLast: [ :delimiter | delimiter isOpener and: delimiter type = #linkName].
]

{ #category : #'as yet unclassified' }
MicInlineSplitter >> pillarFrom: aString [
	"return a collection of pillar nodes which can be used in setChildren: of the owner of aString"
	^ (self start: aString) collect: [ :node | node asPillar ]
]

{ #category : #process }
MicInlineSplitter >> processEmphasis [
	resultArray := self emphasisProcessClass startWithStack: delimiterStack withString: string.
	^ resultArray
]

{ #category : #actions }
MicInlineSplitter >> read: aString [
	incrementation := 1.
	aString ifEmpty: [ ^ self processEmphasis ].
	allDelimiters keys do: [ :key | 
 		(aString beginsWith: key) 
			ifTrue: [ 
				incrementation := key size.
				self delimiterFoundProcess: (allDelimiters at: key).
			].
	].
	self indexIncrement: incrementation.
	^ self read: (aString allButFirst: incrementation)
	
]

{ #category : #public }
MicInlineSplitter >> start: aString [
	delimiterStack := Stack new.
	resultArray := LinkedList new.
	index := 1.
	string := aString.
	^ self read: aString
]
