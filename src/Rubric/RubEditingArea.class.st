"
I am the common class to use for editing text in an area
"
Class {
	#name : #RubEditingArea,
	#superclass : #RubAbstractTextArea,
	#instVars : [
		'wrapped',
		'editorClass'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #'event handling' }
RubEditingArea >> autoScroll: anEvent [
	scrollPane ifNil: [ ^self ].
	self scrollPivot  ifNil: [ ^self ].
	scrollPane scrollWhenMouseMove: anEvent pivotPoint: self scrollPivot
]

{ #category : #initialization }
RubEditingArea >> beForPlainText [
	self editingMode: RubPlainTextMode new
]

{ #category : #initialization }
RubEditingArea >> beForSmalltalkCode [
	self editingMode: RubSmalltalkCodeMode new

]

{ #category : #initialization }
RubEditingArea >> beForSmalltalkCodeInClass: aClass [
	self editingMode: (RubSmalltalkCodeMode new
		classOrMetaClass: aClass;
		yourself)
]

{ #category : #initialization }
RubEditingArea >> beForSmalltalkCodeWithDefaultFont [
	self editingMode: RubSmalltalkCodeMode new.
	self font: StandardFonts defaultFont
]

{ #category : #initialization }
RubEditingArea >> beForSmalltalkComment [
	self editingMode: RubSmalltalkCommentMode new
]

{ #category : #'public accessing' }
RubEditingArea >> beForSmalltalkScripting [
	self editingMode: RubSmalltalkScriptingMode new
]

{ #category : #'public accessing' }
RubEditingArea >> beForSmalltalkScriptingWithDefaultFont [
	self editingMode: RubSmalltalkScriptingMode new.
	self font: StandardFonts defaultFont
]

{ #category : #'public accessing' }
RubEditingArea >> beNotWrapped [
	self wrapped
		ifTrue: [ self handleParagraphChange: [ self wrapped: false ] ]
]

{ #category : #'public accessing' }
RubEditingArea >> beWrapped [
	self wrapped
		ifFalse: [ self handleParagraphChange: [ self wrapped: true ] ]
]

{ #category : #binding }
RubEditingArea >> bindingOf: aString [
	^ self editingMode bindingOf: aString
]

{ #category : #styler }
RubEditingArea >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"
	self shoutStyler ifNotNil: [ :styler | styler classOrMetaClass: aBehavior]
]

{ #category : #'error handling' }
RubEditingArea >> doesNotUnderstand: aMessage [
	"{aMessage . thisContext sender} inspectOnce."
	^ aMessage sendTo: self editingMode
]

{ #category : #accessing }
RubEditingArea >> editorClass [
	^ editorClass ifNil: [ editorClass := RubTextEditor ]
]

{ #category : #accessing }
RubEditingArea >> editorClass: aClass [
	editorClass := aClass
]

{ #category : #highlighting }
RubEditingArea >> ensureAndGetNewHighlightSegmentFrom: startIndex to: stopIndex [
	
	^ self 
		removeHighlightSegment;
		addSegment: (RubHighlightSegmentMorph from: startIndex to: stopIndex)
]

{ #category : #highlighting }
RubEditingArea >> ensureAndGetNewHoverHighlightSegmentFrom: startIndex to: stopIndex [
	
	^ self 
		removeHoverHighlightSegment;
		addSegment: (RubHoverHighlightSegmentMorph from: startIndex to: stopIndex)
]

{ #category : #highlighting }
RubEditingArea >> getHighlightSegment [

	^ self getSegmentOfClass:  RubHighlightSegmentMorph
]

{ #category : #highlighting }
RubEditingArea >> getHoverHighlightSegment [

	^ self getSegmentOfClass: RubHoverHighlightSegmentMorph
]

{ #category : #highlighting }
RubEditingArea >> getSegmentOfClass: aClass [

	^ self segments detect: [ :each | each class = aClass ] ifNone: [ nil ]
]

{ #category : #highlighting }
RubEditingArea >> hasHighlightSegment [

	^ self getHighlightSegment notNil
]

{ #category : #'event handling' }
RubEditingArea >> mouseMove: evt [
	| editEvt |
	super mouseMove: evt.
	scrollPane ifNil: [ ^ self ].
	self scrollPivot: evt cursorPoint.
	editEvt := evt transformedBy: (self transformedFrom: scrollPane ) inverseTransformation.
	(editEvt position y between: scrollPane top and: scrollPane bottom)
		ifFalse: [
			self
				startStepping: #autoScroll:
				at: Time millisecondClockValue
				arguments: (Array with: evt)
				stepTime: 100	"fast enough" ]
		ifTrue: [ self stopSteppingSelector: #autoScroll: ]
]

{ #category : #private }
RubEditingArea >> newParagraph [
	^ RubCurrentLineBarDecorator	next: (RubExtraSelectionDecorator next: super newParagraph)
]

{ #category : #highlighting }
RubEditingArea >> removeHighlightSegment [

	(self getHighlightSegment)
		ifNotNil: [ :aSegment | aSegment delete ].
]

{ #category : #highlighting }
RubEditingArea >> removeHoverHighlightSegment [

	(self getHoverHighlightSegment)
		ifNotNil: [ :aSegment | aSegment delete ].
]

{ #category : #styler }
RubEditingArea >> shoutAboutToStyle: myself [

	^self editingMode shoutAboutToStyle: myself
]

{ #category : #styler }
RubEditingArea >> shoutStyler [
	^ self decoratorNamed: #shoutStyler
]

{ #category : #styler }
RubEditingArea >> textArea [
	"for #whenTextAreaSelectionChanged:"
	^self
]

{ #category : #settings }
RubEditingArea >> walkAlongDisplayedLine [
	^ self class walkAlongDisplayedLine
]

{ #category : #'accessing decorators' }
RubEditingArea >> withSecondarySelection [
	self withDecoratorNamed: #secondarySelection
]

{ #category : #'accessing decorators' }
RubEditingArea >> withSelectionBar [
	self withDecoratorNamed: #selectionBar
]

{ #category : #'accessing decorators' }
RubEditingArea >> withoutOpeningClosingDelimitersHighlight [
	self withoutDecoratorNamed: #openingClosingDelimitersHighlight
]

{ #category : #'accessing decorators' }
RubEditingArea >> withoutSelectionBar [
	self withoutDecoratorNamed: #selectionBar
]

{ #category : #styler }
RubEditingArea >> withoutShoutStyler [
	self withoutDecoratorNamed: #shoutStyler
]

{ #category : #accessing }
RubEditingArea >> wrapped [
	^ wrapped ifNil: [ wrapped := false ] 
]

{ #category : #accessing }
RubEditingArea >> wrapped: aBoolean [
	wrapped := aBoolean
]
