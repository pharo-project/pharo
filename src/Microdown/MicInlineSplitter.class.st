Class {
	#name : #MicInlineSplitter,
	#superclass : #Object,
	#instVars : [
		'openersStack',
		'result',
		'allDelimiters',
		'index',
		'string',
		'incrementation',
		'delimiterSubclassFound',
		'children',
		'nestedLevel',
		'opener',
		'closer',
		'correctSubstring',
		'correctURL'
	],
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicInlineSplitter >> abstractDelimiterClass [
	^ MicAbstractDelimiter
]

{ #category : #'handle basic text' }
MicInlineSplitter >> addABasicTextFrom: start to: end toFinalArray: aFinalArray [
	start > end
		ifTrue: [ " do nothing "]
		ifFalse: [ aFinalArray add: (self newBasicInlineBlockFrom: start to: end ) ]
]

{ #category : #process }
MicInlineSplitter >> addInlineBlock: indexOfAssociateOpener [
	| startIndex endIndex |
	opener := openersStack at: indexOfAssociateOpener.
	closer := delimiterSubclassFound index: index.
	"Check the linkName or figure case"
	(#(#linkName #figure) includes: opener type ) 
		ifTrue: [ self linkOrFigureProcess: indexOfAssociateOpener ]
		ifFalse: [ 
			startIndex := opener index + opener size.
			endIndex := closer index - 1.
			correctSubstring := string collect: [ :c | c ] from: startIndex to: endIndex.
			"Delete openers above in stack, considered as unclosed so ignored"
			self popFrom: 1 to: indexOfAssociateOpener.
			"Add found inline block"
			openersStack size > 0
				ifTrue: [ 
					(openersStack size > nestedLevel)
						ifTrue: [ 
							children 
								ifEmpty: [ children add: self newInlineBlock ]
								ifNotEmpty: [ children last add: self newInlineBlock ] ]
						ifFalse: [ children add: self newInlineBlockWithoutChildren ].
				]
				ifFalse: [ 
					result add: self newInlineBlock.
					children := LinkedList new.
				].
			nestedLevel := openersStack size.
		]
]

{ #category : #accessing }
MicInlineSplitter >> allDelimiters [
	^ allDelimiters
]

{ #category : #process }
MicInlineSplitter >> bothCase [
	| indexOfAssociateOpener |
	indexOfAssociateOpener := openersStack findFirst: [ :each | delimiterSubclassFound type = each type ].
	(openersStack isEmpty or: [ indexOfAssociateOpener = 0])
		ifTrue: [ 
			"We consider the delimiter as a new opener or it's the first opener"
			self pushNewOpener ]
		ifFalse: [ 
			"We consider the delimiter as a closer of last opened inline block"
			self addInlineBlock: indexOfAssociateOpener ]
]

{ #category : #process }
MicInlineSplitter >> closerOnlyCase [
	"IF delimiter found is a closer which can close an opened inline block
	THEN we add the associate inline block
	ELSE we ignore it"
	| indexOfAssociateOpener typesToFind|
	typesToFind := (#(#linkName #figure) includes: delimiterSubclassFound type ) 
		ifTrue: [#(#linkName #figure)] 
		ifFalse: [ Array braceWith: delimiterSubclassFound type ].
	indexOfAssociateOpener := openersStack findFirst: [ :each | typesToFind includes: each type ].
	(indexOfAssociateOpener > 0) ifTrue: [ self addInlineBlock: indexOfAssociateOpener ]
]

{ #category : #process }
MicInlineSplitter >> delimiterFoundProcess [
	"Case 0: it's an escape character"
	(delimiterSubclassFound type = #escape) 
		ifTrue: [ incrementation := incrementation + 1 ]
		ifFalse: [ 
			"Case 1: it's a opener only"
			(delimiterSubclassFound isOpenerOnly) 
				ifTrue: [ self openerOnlyCase ]
				ifFalse: [ 
					"Case 2: it's both opener and closer"
					(delimiterSubclassFound isBoth) 
						ifTrue: [ self bothCase ]
						ifFalse: [ 
							"Case 3: it's a closer only"
							self closerOnlyCase
						]
				]
		]
			
			

]

{ #category : #actions }
MicInlineSplitter >> indexIncrement [
	^ self indexIncrement: 1
]

{ #category : #actions }
MicInlineSplitter >> indexIncrement: anInteger [
	index := index + anInteger
]

{ #category : #initialization }
MicInlineSplitter >> initialize [
	super initialize.
	self initializeDelimiters.
	openersStack := Stack new.
	result := LinkedList new.
	children := LinkedList new.
	nestedLevel := 0.
	index := 1
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

{ #category : #'handle basic text' }
MicInlineSplitter >> insertBasicText: anArrayOfInlineBlocks [
	^ self insertBasicText: anArrayOfInlineBlocks withFinalArray: LinkedList new from: 1 to: string size
]

{ #category : #'handle basic text' }
MicInlineSplitter >> insertBasicText: anArrayOfInlineBlocks from: startIndex to: endIndex [
	^ self insertBasicText: anArrayOfInlineBlocks withFinalArray: LinkedList new from: startIndex to: endIndex
]

{ #category : #'handle basic text' }
MicInlineSplitter >> insertBasicText: anArrayOfInlineBlocks withFinalArray: finalArray from: startIndex to: endIndex [
	anArrayOfInlineBlocks ifEmpty: [ ^ finalArray asArray ].
	(anArrayOfInlineBlocks size = 1) ifTrue: [ 
		| element |
		element := anArrayOfInlineBlocks first.
		finalArray ifEmpty: [ self addABasicTextFrom: startIndex to: element start - 1 toFinalArray: finalArray ].
		finalArray add: element.
		self addABasicTextFrom: element end + 1 to: endIndex toFinalArray: finalArray.
		^ self insertBasicText: Array empty withFinalArray: finalArray from: startIndex to: endIndex
	].
	finalArray ifEmpty: [ self addABasicTextFrom: startIndex to: anArrayOfInlineBlocks first start - 1 toFinalArray: finalArray ].
	finalArray add: anArrayOfInlineBlocks first.
	self addABasicTextFrom: anArrayOfInlineBlocks first end + 1 to: anArrayOfInlineBlocks second start - 1 toFinalArray: finalArray.
	^ self insertBasicText: anArrayOfInlineBlocks allButFirst withFinalArray: finalArray from: startIndex to: endIndex
	
	
	
]

{ #category : #process }
MicInlineSplitter >> linkOrFigureProcess: indexOfAssociateOpener [
	"IF we find a ( just after and a ) after again
	THEN we add the associate link or figure inline block
	ELSE we ignore it"
	| startIndex endIndex urlCloserIndex |
	((string allButFirst: (closer index + closer size - 1)) beginsWith: self urlOpenerDelimiterClass markup)
		ifTrue: [ 
			urlCloserIndex := string indexOfSubCollection: self urlCloserDelimiterClass markup startingAt: closer index ifAbsent: [ 0 ].
			(urlCloserIndex > 0) 
				ifTrue: [ 
					startIndex := opener index + opener size.
					endIndex := closer index - 1.
					correctSubstring := string collect: [ :c | c ] from: startIndex to: endIndex.
					correctURL := string collect: [ :c | c ] from: (closer index + closer size + self urlOpenerDelimiterClass size) to: (urlCloserIndex - 1).
					"Delete openers above in stack, considered as unclosed so ignored"
					self popFrom: 1 to: indexOfAssociateOpener.
					"Add found inline block"
			(openersStack size > 0)
				ifTrue: [ 
					(openersStack size > nestedLevel)
						ifTrue: [ 
							children 
								ifEmpty: [ children add: (self newURLInlineBlockWithCloser: urlCloserIndex) ]
								ifNotEmpty: [ children last add: (self newURLInlineBlockWithCloser: urlCloserIndex) ] ]
						ifFalse: [ children add: (self newURLInlineBlockWithoutChildrenWithCloser: urlCloserIndex) ].
				]
				ifFalse: [ 
					result add: (self newURLInlineBlockWithCloser: urlCloserIndex).
					children := LinkedList new.
				].
			incrementation := urlCloserIndex - index.
			nestedLevel := openersStack size.
				]
				ifFalse: [ "do nothing" ]
		]
		ifFalse: [ "do nothing" ]
]

{ #category : #'instance creation' }
MicInlineSplitter >> newBasicInlineBlockFrom: stIndex to: eIndex [
	^ (MicBasicInlineBlock
			from: stIndex
			to: eIndex
			withKind: #basic
			withSubstring: (string collect: [ :c | c ] from: stIndex to: eIndex)
			withChildren: Array empty)
]

{ #category : #'instance creation' }
MicInlineSplitter >> newInlineBlock [
	| inlineBlockClass |
	inlineBlockClass := opener associatedInlineBlock.
	(inlineBlockClass isEvaluated) 
		ifTrue: [ 
			^ inlineBlockClass
				from: opener index
				to: closer index + closer size - 1
				withKind: opener type
				withSubstring: correctSubstring
				withChildren: children asArray
		 ]
		ifFalse: [ ^ self newInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineSplitter >> newInlineBlockWithCloser: aCloserIndex [
	| inlineBlockClass |
	inlineBlockClass := opener associatedInlineBlock.
	(inlineBlockClass isEvaluated) 
		ifTrue: [ 
			^ inlineBlockClass
				from: opener index
				to: aCloserIndex
				withKind: opener type
				withSubstring: correctSubstring
				withChildren: children asArray
		 ]
		ifFalse: [ ^ self newInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineSplitter >> newInlineBlockWithoutChildren [
	^ opener associatedInlineBlock
			from: opener index
			to: closer index + closer size - 1
			withKind: opener type
			withSubstring: correctSubstring
]

{ #category : #'instance creation' }
MicInlineSplitter >> newInlineBlockWithoutChildrenWithCloser: aCloserIndex [
	^ opener associatedInlineBlock
			from: opener index
			to: aCloserIndex
			withKind: opener type
			withSubstring: correctSubstring
]

{ #category : #'instance creation' }
MicInlineSplitter >> newURLInlineBlock [
	| inlineBlockClass |
	inlineBlockClass := opener associatedInlineBlock.
	(inlineBlockClass isEvaluated) 
		ifTrue: [ 
			^ inlineBlockClass
				from: opener index
				to: closer index + closer size - 1
				withKind: opener type
				withSubstring: correctSubstring
				withChildren: children asArray
				withURL: correctURL
		 ]
		ifFalse: [ ^ self newURLInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineSplitter >> newURLInlineBlockWithCloser: aCloserIndex [
	| inlineBlockClass |
	inlineBlockClass := opener associatedInlineBlock.
	(inlineBlockClass isEvaluated) 
		ifTrue: [ 
			^ inlineBlockClass
				from: opener index
				to: aCloserIndex
				withKind: opener type
				withSubstring: correctSubstring
				withChildren: children asArray
				withURL: correctURL
		 ]
		ifFalse: [ ^ self newURLInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineSplitter >> newURLInlineBlockWithoutChildren [
	^ opener associatedInlineBlock
			from: opener index
			to: closer index + closer size - 1
			withKind: opener type
			withSubstring: correctSubstring
			withURL: correctURL
]

{ #category : #'instance creation' }
MicInlineSplitter >> newURLInlineBlockWithoutChildrenWithCloser: aCloserIndex [
	^ opener associatedInlineBlock
			from: opener index
			to: aCloserIndex
			withKind: opener type
			withSubstring: correctSubstring
			withURL: correctURL
]

{ #category : #process }
MicInlineSplitter >> openerOnlyCase [
	self pushNewOpener
]

{ #category : #actions }
MicInlineSplitter >> popFrom: aStartIndex to: anEndIndex [
	aStartIndex to: anEndIndex do: [ :i | openersStack pop ]
	
]

{ #category : #actions }
MicInlineSplitter >> pushNewOpener [
	openersStack push: (delimiterSubclassFound index: index)
]

{ #category : #actions }
MicInlineSplitter >> read: aString [
	incrementation := 1.
	aString ifEmpty: [ ^ self resultProcess ].
	allDelimiters keys do: [ :key | 
 		(aString beginsWith: key) 
			ifTrue: [ 
				incrementation := key size.
				delimiterSubclassFound := (allDelimiters at: key).
				self delimiterFoundProcess
			].
	].
	self indexIncrement: incrementation.
	^ self read: (aString allButFirst: incrementation)
	
]

{ #category : #process }
MicInlineSplitter >> resultProcess [
	"At the end, we add all basic text between found inline blocks"
	result ifEmpty: [ ^ { (self newBasicInlineBlockFrom: 1 to: string size)} ].
	result do: [ :e | 
		| startSubstring endSubstring |
		startSubstring := string indexOfSubCollection: e substring startingAt: e start.
		endSubstring := startSubstring + e substring size - 1 .
		e children: (self insertBasicText: e children from: startSubstring to: endSubstring) ].
	^ self insertBasicText: result
]

{ #category : #public }
MicInlineSplitter >> start: aString [
	aString ifEmpty: [ ^ Array empty ].
	string := aString.
	^ self read: aString
]

{ #category : #accessing }
MicInlineSplitter >> urlCloserDelimiterClass [
	^ MicURLCloserDelimiter
]

{ #category : #accessing }
MicInlineSplitter >> urlOpenerDelimiterClass [
	^ MicURLOpenerDelimiter 
]
