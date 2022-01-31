"
I'm a parser of block text. I'm responsible to generate text, annotation, format, link, url kind of objects.
"
Class {
	#name : #MicInlineParser,
	#superclass : #Object,
	#instVars : [
		'openersStack',
		'result',
		'allDelimiters',
		'index',
		'string',
		'incrementation',
		'children',
		'nestedLevel',
		'opener',
		'closer',
		'correctSubstring',
		'correctURL',
		'keys',
		'delimiterClass'
	],
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicInlineParser >> abstractDelimiterClass [
	^ MicAbstractDelimiter
]

{ #category : #'handle basic text' }
MicInlineParser >> addATextFrom: start to: end toFinalArray: aFinalArray [
	start > end
		ifTrue: [ " do nothing "]
		ifFalse: [ aFinalArray add: (self newBasicInlineBlockFrom: start to: end ) ]
]

{ #category : #process }
MicInlineParser >> addInlineBlock: indexOfAssociateOpener [
	| startIndex endIndex |
	opener := openersStack at: indexOfAssociateOpener.
	closer := delimiterClass index: index.
	"Check the linkName or figure case"
	(#(#linkName #figure) includes: opener type ) 
		ifTrue: [ self linkOrFigureProcess: indexOfAssociateOpener ]
		ifFalse: [ 
			startIndex := opener index + opener size.
			endIndex := closer index - 1.
			correctSubstring := string copyFrom: startIndex to: endIndex.
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
MicInlineParser >> allDelimiters [
	^ allDelimiters
]

{ #category : #process }
MicInlineParser >> bothCase [
	| indexOfAssociateOpener |
	indexOfAssociateOpener := openersStack findFirst: [ :each | delimiterClass type = each type ].
	(openersStack isEmpty or: [ indexOfAssociateOpener = 0])
		ifTrue: [ 
			"We consider the delimiter as a new opener or it's the first opener"
			self pushNewOpener ]
		ifFalse: [ 
			"We consider the delimiter as a closer of last opened inline block"
			self addInlineBlock: indexOfAssociateOpener ]
]

{ #category : #process }
MicInlineParser >> closerOnlyCase [
	"IF delimiter found is a closer which can close an opened inline block
	THEN we add the associate inline block
	ELSE we ignore it"
	| indexOfAssociateOpener typesToFind|
	typesToFind := (#(#linkName #figure) includes: delimiterClass type ) 
		ifTrue: [#(#linkName #figure)] 
		ifFalse: [ Array braceWith: delimiterClass type ].
	indexOfAssociateOpener := openersStack findFirst: [ :each | typesToFind includes: each type ].
	(indexOfAssociateOpener > 0) ifTrue: [ self addInlineBlock: indexOfAssociateOpener ]
]

{ #category : #process }
MicInlineParser >> delimiterFoundProcess [
	"Case 1: it's a opener only"
	(delimiterClass isOpenerOnly) 
		ifTrue: [ self openerOnlyCase ]
		ifFalse: [ 
					"Case 2: it's both opener and closer"
			(delimiterClass isBoth) 
				ifTrue: [ self bothCase ]
				ifFalse: [ 
							"Case 3: it's a closer only"
							self closerOnlyCase
				]
			]
	
			
			

]

{ #category : #actions }
MicInlineParser >> identifyMarkupFor: aString [

	keys do: [ :key |
		(aString beginsWith: key) ifTrue: [ 
			incrementation := key size.
			delimiterClass := allDelimiters at: key.
			self delimiterFoundProcess.
			^ self ] ]
]

{ #category : #actions }
MicInlineParser >> indexIncrement [
	^ self indexIncrement: 1
]

{ #category : #actions }
MicInlineParser >> indexIncrement: anInteger [
	index := index + anInteger
]

{ #category : #initialization }
MicInlineParser >> initialize [
	super initialize.
	self initializeDelimiters.
	openersStack := Stack new.
	result := LinkedList new.
	children := LinkedList new.
	nestedLevel := 0.
	index := 1.

]

{ #category : #initialization }
MicInlineParser >> initializeDelimiters [
	allDelimiters := Dictionary new.
	self abstractDelimiterClass subclasses 
		select: [ :subclass | subclass isActive ]
		thenDo: [ :subclass | allDelimiters 
										at: subclass markup 
										put: subclass ].
	keys := allDelimiters keys.
]

{ #category : #'handle basic text' }
MicInlineParser >> insertBasicText: anArrayOfInlineBlocks [
	^ self insertBasicText: anArrayOfInlineBlocks withFinalArray: LinkedList new from: 1 to: string size
]

{ #category : #'handle basic text' }
MicInlineParser >> insertBasicText: anArrayOfInlineBlocks from: startIndex to: endIndex [
	^ self insertBasicText: anArrayOfInlineBlocks withFinalArray: LinkedList new from: startIndex to: endIndex
]

{ #category : #'handle basic text' }
MicInlineParser >> insertBasicText: anArrayOfInlineBlocks 
	withFinalArray: finalArray 
	from: startIndex 
	to: endIndex [
	
	anArrayOfInlineBlocks ifEmpty: [ ^ finalArray asArray ].
	(anArrayOfInlineBlocks size = 1) ifTrue: [ 
		| element |
		element := anArrayOfInlineBlocks first.
		finalArray ifEmpty: [ self addATextFrom: startIndex 
										to: element start - 1 
										toFinalArray: finalArray ].
		finalArray add: element.
		self addATextFrom: element end + 1 
			to: endIndex 
			toFinalArray: finalArray.
		^ self insertBasicText: Array empty withFinalArray: finalArray from: startIndex to: endIndex
	].
	finalArray ifEmpty: [ 
		self addATextFrom: startIndex 
			to: anArrayOfInlineBlocks first start - 1 
			toFinalArray: finalArray ].
	finalArray add: anArrayOfInlineBlocks first.
	self 
		addATextFrom: anArrayOfInlineBlocks first end + 1 
		to: anArrayOfInlineBlocks second start - 1 
		toFinalArray: finalArray.

	^ self insertBasicText: anArrayOfInlineBlocks allButFirst 
		withFinalArray: finalArray 
		from: startIndex 
		to: endIndex
	
	
	
]

{ #category : #process }
MicInlineParser >> linkOrFigureProcess: indexOfAssociateOpener [
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
					correctSubstring := string copyFrom: startIndex to: endIndex.
					correctURL := string copyFrom: (closer index + closer size + self urlOpenerDelimiterClass size) to: (urlCloserIndex - 1).
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
MicInlineParser >> newBasicInlineBlockFrom: stIndex to: eIndex [
	^ (MicTextBlock
			from: stIndex
			to: eIndex
			withSubstring: (string copyFrom: stIndex to: eIndex)
			withChildren: Array empty)
			
		"should use copyFrom:to:"
]

{ #category : #'instance creation' }
MicInlineParser >> newInlineBlock [
	| inlineBlockClass |
	inlineBlockClass := opener associatedInlineBlock.
	(inlineBlockClass isEvaluated) 
		ifTrue: [ 
			^ inlineBlockClass
				from: opener index
				to: closer index + closer size - 1
				withSubstring: correctSubstring
				withChildren: children asArray
		 ]
		ifFalse: [ ^ self newInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineParser >> newInlineBlockWithoutChildren [
	^ opener associatedInlineBlock
			from: opener index
			to: closer index + closer size - 1
			withSubstring: correctSubstring
			
]

{ #category : #'instance creation' }
MicInlineParser >> newURLInlineBlockWithCloser: aCloserIndex [
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
				withParser: self
		 ]
		ifFalse: [ ^ self newURLInlineBlockWithoutChildren ]
	
]

{ #category : #'instance creation' }
MicInlineParser >> newURLInlineBlockWithoutChildren [
	^ opener associatedInlineBlock
			from: opener index
			to: closer index + closer size - 1
			withKind: opener type
			withSubstring: correctSubstring
			withURL: correctURL
]

{ #category : #'instance creation' }
MicInlineParser >> newURLInlineBlockWithoutChildrenWithCloser: aCloserIndex [
	^ opener associatedInlineBlock
			from: opener index
			to: aCloserIndex
			withKind: opener type
			withSubstring: correctSubstring
			withURL: correctURL
]

{ #category : #process }
MicInlineParser >> openerOnlyCase [
	self pushNewOpener
]

{ #category : #public }
MicInlineParser >> parse: aString [

	aString ifEmpty: [ ^ Array empty ].
	string := aString.
	^ self read: aString
]

{ #category : #actions }
MicInlineParser >> popFrom: aStartIndex to: anEndIndex [
	aStartIndex to: anEndIndex do: [ :i | openersStack pop ]
	
]

{ #category : #actions }
MicInlineParser >> pushNewOpener [
	openersStack push: (delimiterClass index: index)
]

{ #category : #actions }
MicInlineParser >> read: aString [
	| next |
	incrementation := 1.
	aString ifEmpty: [ ^ self resultProcess ].
	self identifyMarkupFor: aString.
	"this is stupid to iterate all. We can stop as soon as we found one. "

	aString first = $\ ifTrue: [ 
		incrementation := incrementation + 1 ].
	self indexIncrement: incrementation.
	aString size > incrementation 
		ifTrue: [ next := (aString allButFirst: incrementation)]
		ifFalse: [next := ''].
	^ self read: next.
]

{ #category : #process }
MicInlineParser >> resultProcess [
	"At the end, we add all basic text between found inline blocks"
	
	result ifEmpty: [ ^ { (self newBasicInlineBlockFrom: 1 to: string size)} ].
	result do: [ :e | 
		| startSubstring endSubstring |
		e substring isEmpty 	
			ifTrue: [ startSubstring := endSubstring := 1 ]
			ifFalse: [  	startSubstring := (string indexOfSubCollection: e substring startingAt: e start) max: 1.
		endSubstring := startSubstring + e substring size - 1 ].
		e children: (self insertBasicText: e children from: startSubstring to: endSubstring) ].
	^ self insertBasicText: result
]

{ #category : #public }
MicInlineParser >> start: aString [
	self deprecated: 'Use parse: instead' transformWith: '`@receiver start: `@arg' -> '`@receiver parse: `@arg'.
	^ self parse: aString
]

{ #category : #accessing }
MicInlineParser >> urlCloserDelimiterClass [
	^ MicURLCloserDelimiter
]

{ #category : #accessing }
MicInlineParser >> urlOpenerDelimiterClass [
	^ MicURLOpenerDelimiter 
]
