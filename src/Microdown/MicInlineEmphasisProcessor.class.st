"
I am a recursive algorithm to find different kinds of emphasis i.e., inline formats in a text line.

To proceed, call my instance creation methods which start automatically the emphasis process.

I need:
- stack = stack of delimiters determined by the inline splitter algorithm
- string = the initial text line

At the end, I return an array of MicInlineBlock which represents the initial text line.
"
Class {
	#name : #MicInlineEmphasisProcessor,
	#superclass : #Object,
	#instVars : [
		'stack',
		'result',
		'string',
		'start',
		'end',
		'startIndex',
		'endIndex',
		'firstOpener',
		'firstOpenerIndex',
		'associateCloser',
		'associateCloserIndex',
		'children'
	],
	#category : #'Microdown-Parser'
}

{ #category : #'new & start' }
MicInlineEmphasisProcessor class >> startWithStack: aStack withResult: aResultArray withStart: aStartSubstringIndex withEnd: anEndSubstringIndex withString: aString [

	| newProcessor |
	newProcessor := self new 
		stack: aStack; 
		result: aResultArray; 
		start: aStartSubstringIndex; 
		end: anEndSubstringIndex; 
		string: aString; yourself.
	^ newProcessor processEmphasis
]

{ #category : #'new & start' }
MicInlineEmphasisProcessor class >> startWithStack: aStack withString: aString [
	^ self startWithStack: aStack 
		withResult: LinkedList new 
		withStart: 1 
		withEnd: aString size 
		withString: aString
]

{ #category : #actions }
MicInlineEmphasisProcessor >> addBasicTextBlocks [
	| stringArray |
	stringArray := string asArray.
	stringArray doWithIndex: [ :each :i | (i between: start and: end) ifFalse: [ stringArray at: i put: nil ]  ].
	result do: [ :anInlineBlock | anInlineBlock start to: anInlineBlock end do: [ :k | stringArray at: k put: nil ] ].
	stringArray splitOn: nil indicesDo: [ :stIndex :eIndex | 
		| substring |
		substring := string collect: [ :c | c ] from: stIndex to: eIndex.
		substring isEmpty ifFalse: [ 
			result add: 
				(MicBasicInlineBlock
					from: stIndex
					to: eIndex
					withKind: #basic
					withSubstring: substring
					withChildren: #())
			 ]
	].
	^ (result sort: [ :block :block2 | block start < block2 start ]) asArray
]

{ #category : #actions }
MicInlineEmphasisProcessor >> childrenInNestedCase [
	^ self class 
		startWithStack: (stack withIndexSelect: [:value :i | i between: firstOpenerIndex + 1 and: associateCloserIndex - 1 ]) 
		withResult: LinkedList new 
		withStart: startIndex 
		withEnd: endIndex 
		withString: string
]

{ #category : #accessing }
MicInlineEmphasisProcessor >> end: anEndIndex [
	end := anEndIndex 
]

{ #category : #'global process' }
MicInlineEmphasisProcessor >> linkOrFigureProcess [
	| urlCloserStringIndex correctSubstring correctURL |
	"1. search the linkname closer delimiter"
	associateCloserIndex := (stack allButFirst: firstOpenerIndex) findFirst: [ :delimiter | delimiter isCloser and: delimiter type = #linkName].
	associateCloserIndex isZero 
		ifTrue: [ 
			self stackWithoutFirstOpenerIndex.
			^ self processEmphasis ].
	"2. translate index in delimiters stack"
	associateCloserIndex := associateCloserIndex + firstOpenerIndex.
	associateCloser := stack at: associateCloserIndex.
	"3. check we find a ( just after closer in string"
	((string allButFirst: associateCloser index + associateCloser size - 1) beginsWith: self urlOpenerDelimiterClass markup) 
		ifFalse: [ self stackWithoutFirstOpenerIndex. ^ self processEmphasis ].
	"4. search url closer"
	urlCloserStringIndex := string indexOfSubCollection: self urlCloserDelimiterClass markup startingAt: associateCloser index 
		ifAbsent: [ self stackWithoutFirstOpenerIndex. ^ self processEmphasis ].
	"5. link or figure found"
	startIndex := firstOpener index + firstOpener size.
	endIndex := associateCloser index - 1.
	correctSubstring := string collect: [ :c | c ] from: startIndex to: endIndex.
	correctURL := string collect: [ :c | c ] from: (associateCloser index + associateCloser size + self urlOpenerDelimiterClass size) to: (urlCloserStringIndex - 1).
	"6. define children"
	self processNestedCase.
	result add: 
		(firstOpener associatedInlineBlock
			from: firstOpener index
			to: urlCloserStringIndex 
			withKind: firstOpener type
			withSubstring: correctSubstring
			withChildren: children
			withURL: correctURL).
	"7. keep going"
	stack := (stack withIndexSelect: [:value :i | (i between: firstOpenerIndex and: urlCloserStringIndex) not ]).
	^ self processEmphasis
	
	
	
	
]

{ #category : #'global process' }
MicInlineEmphasisProcessor >> processEmphasis [
	| correctSubstring |
	"1. search first opener. If not found, exit case."
	firstOpenerIndex := stack findFirst: [ :delimiter | delimiter isOpener ].
	firstOpenerIndex isZero 
		ifTrue: [ ^ self addBasicTextBlocks ].
	firstOpener := stack at: firstOpenerIndex.
	"Case link or figure"
	(#(#linkName #figure) includes: firstOpener type ) ifTrue: [ ^ self linkOrFigureProcess ].
	"2. search first closer with same type, just after. If not found, pair isn't complete so we skip it and keep going."	
	associateCloserIndex := (stack allButFirst: firstOpenerIndex) findFirst: [ :delimiter | delimiter isCloser and: delimiter type = firstOpener type].
	associateCloserIndex isZero 
		ifTrue: [ 
			self stackWithoutFirstOpenerIndex.
			^ self processEmphasis ].
	"3. translate index in delimiters stack"
	associateCloserIndex := associateCloserIndex + firstOpenerIndex.
	associateCloser := stack at: associateCloserIndex.	
	"4. set substring in global string concerned by found emphasis"
	startIndex := firstOpener index + firstOpener size.
	endIndex := associateCloser index - 1.
	correctSubstring := string collect: [ :c | c ] from: startIndex to: endIndex.
	"5. are we in a nested emphasis case?"
	self processNestedCase.
	"6. add in result_array"
	result add: 
		(firstOpener associatedInlineBlock
			from: firstOpener index
			to: associateCloser index + associateCloser size - 1
			withKind: firstOpener type
			withSubstring: correctSubstring
			withChildren: children).
	"7. keep going"
	stack := (stack withIndexSelect: [:value :i | (i between: firstOpenerIndex and: associateCloserIndex) not ]).
	^ self processEmphasis
	
		
]

{ #category : #actions }
MicInlineEmphasisProcessor >> processNestedCase [
	children := ((associateCloserIndex = (firstOpenerIndex + 1)) or: (firstOpener associatedInlineBlock isEvaluated) not)
		ifTrue: [ #() ]
		ifFalse: [ self childrenInNestedCase ]
]

{ #category : #accessing }
MicInlineEmphasisProcessor >> result: anArray [
	result := anArray
]

{ #category : #accessing }
MicInlineEmphasisProcessor >> stack: aStack [
	stack := aStack
]

{ #category : #actions }
MicInlineEmphasisProcessor >> stackWithoutFirstOpenerIndex [
	stack := (stack withIndexSelect: [ :value :i | (i = firstOpenerIndex) not ])
]

{ #category : #accessing }
MicInlineEmphasisProcessor >> start: aStartIndex [
	start := aStartIndex
]

{ #category : #accessing }
MicInlineEmphasisProcessor >> string: aString [
	string := aString
]

{ #category : #'class references' }
MicInlineEmphasisProcessor >> urlCloserDelimiterClass [
	^ MicURLCloserDelimiter
]

{ #category : #'class references' }
MicInlineEmphasisProcessor >> urlOpenerDelimiterClass [
	^ MicURLOpenerDelimiter 
]
