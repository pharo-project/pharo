"
I'm an abstract visitor for Microdown document three. 
By default I visit only the nodes of the domain: it means that I will not visit the label of an annotated paragraph 
or the parameters of an annotation or the URL of a figure because such elements are not represented by subclasses 
of MicElement.

"
Class {
	#name : #MicrodownVisitor,
	#superclass : #Object,
	#category : #'Microdown-Visitor'
}

{ #category : #'visiting main API' }
MicrodownVisitor >> visit: aMicElement [

	^ aMicElement accept: self
]

{ #category : #'visiting main API' }
MicrodownVisitor >> visitAll: aCollection [

	aCollection do: [ :each | each accept: self ]
]

{ #category : #visiting }
MicrodownVisitor >> visitAnchor: anAnchor [
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitAnchorReference: anAnchorReference [
]

{ #category : #visiting }
MicrodownVisitor >> visitAnnotated: anAnnotated [

	^ anAnnotated body do: [ :each | each accept: self ]
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitAnnotation: anAnnotation [
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitBold: aFormat [

	self visitChildrenOf: aFormat.

]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitChildrenOf: aFormat [

	aFormat children do: [ :each | each accept: self ]
]

{ #category : #'visiting - extensions' }
MicrodownVisitor >> visitCitation: aVisitor [
]

{ #category : #visiting }
MicrodownVisitor >> visitCode: aCodeBlock [ 
	 
	^ aCodeBlock captionElements do: [ :each |each accept: self ]

]

{ #category : #visiting }
MicrodownVisitor >> visitComment: aComment [
]

{ #category : #visiting }
MicrodownVisitor >> visitEnvironment: anEnvironment [

	^ anEnvironment bodyElements do: [ :each | each accept: self ]
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitFigure: aFigure [
	
	^ aFigure captionElements do: [:each | each accept: self ]
		
]

{ #category : #visiting }
MicrodownVisitor >> visitFootnote: aFootnote [
	"subclassResponsibility"
]

{ #category : #visiting }
MicrodownVisitor >> visitHeader: aHeader [

	^  aHeader headerElements do: [ :each | each accept: self ]
]

{ #category : #visiting }
MicrodownVisitor >> visitHorizontalLine: anHorizontalLineBlock [
]

{ #category : #visiting }
MicrodownVisitor >> visitInputfile: anInputfile [
	"subclassResponsibility"
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitItalic: aFormat [

	self visitChildrenOf: aFormat.

]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitLink: aLink [

	aLink captionElements do: [:each | each accept: self ].

]

{ #category : #'visiting - list' }
MicrodownVisitor >> visitListItem: anItem [

	anItem textElements do: [ :each | each accept: self ].
	self visitChildrenOf: anItem
]

{ #category : #visiting }
MicrodownVisitor >> visitMath: aMicMath [

	aMicMath captionElements do: [ :each | each accept: self ]

]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitMathInline: aMicMathInline [
]

{ #category : #visiting }
MicrodownVisitor >> visitMetaData: aMetaData [
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitMonospace: aFormat [

	self visitChildrenOf: aFormat.

]

{ #category : #'visiting - list' }
MicrodownVisitor >> visitOrderedList: anOrderedList [

	^ anOrderedList children do: [ :each | self visitOrderedListItem: each ]
]

{ #category : #'visiting - list' }
MicrodownVisitor >> visitOrderedListItem: anItem [

	^ self visitListItem: anItem
]

{ #category : #visiting }
MicrodownVisitor >> visitParagraph: aParagraph [

	^ self visitChildrenOf: aParagraph
]

{ #category : #visiting }
MicrodownVisitor >> visitQuote: aQuote [

]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitRaw: aRawFormat [

	
]

{ #category : #visiting }
MicrodownVisitor >> visitRoot: aRoot [

	^ self visitChildrenOf: aRoot
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitStrike: aFormat [

	self visitChildrenOf: aFormat.

]

{ #category : #visiting }
MicrodownVisitor >> visitTable: aTable [
	"subclassResponsibility"
]

{ #category : #'visiting - inline elements' }
MicrodownVisitor >> visitText: aMicTextBlock [

	"do nothing"
]

{ #category : #'visiting - list' }
MicrodownVisitor >> visitUnorderedList: anUnorderedList [

	^ anUnorderedList children do: [ :each | self visitUnorderedListItem: each ]
]

{ #category : #'visiting - list' }
MicrodownVisitor >> visitUnorderedListItem: anUnorderedList [

	^ self visitListItem: anUnorderedList
	
]
