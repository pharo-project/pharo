"
I am a morph useful for visualising differences between two strings.

When a classContext: is setted, the strings are highlighted using such contextual information.
"
Class {
	#name : #DiffMorph,
	#superclass : #ComposableMorph,
	#instVars : [
		'prettyPrint',
		'contextClass',
		'srcMorph',
		'dstMorph',
		'scrollbarMorph',
		'mapMorph',
		'joinMorph',
		'difference',
		'joinMappings',
		'optionsPanel',
		'showOnlyDestination',
		'showOptions',
		'showOnlySource',
		'sourceTextModel',
		'destTextModel',
		'leftCaption',
		'rightCaption'
	],
	#category : #'Tool-Diff'
}

{ #category : #'instance creation' }
DiffMorph class >> from: old to: new [
	"Answer a new instance of the receiver with the given
	old and new text."

	^self new
		from: old
		to: new
]

{ #category : #'instance creation' }
DiffMorph class >> from: old to: new contextClass: aClass [
	"Answer a new instance of the receiver with the given
	old and new text."

	^self new
		from: old
		to: new
		contextClass: aClass
]

{ #category : #initialization }
DiffMorph >> addMainMorphsWith: initialOffset [ 
	| halfWidth leftFraction leftOffset rightFraction rightOffset topOffset |

	topOffset := self addTitlePanel: initialOffset.
	
	halfWidth := self joinMorph width / 2.
	leftFraction := showOnlyDestination
		ifTrue: [ 0 ]
		ifFalse: [ 0.5 ].
		
	leftOffset := showOnlyDestination
		ifTrue: [ 0 ]
		ifFalse: [ halfWidth ].
		
	rightFraction := showOnlySource
		ifTrue: [ 1 ]
		ifFalse: [ 0.5 ].
		
	rightOffset := showOnlySource
		ifTrue: [ 0 ]
		ifFalse: [ halfWidth negated ].
		 
	showOnlyDestination ifFalse: [
		self
			addMorph: self srcMorph
			fullFrame: (LayoutFrame identity
				rightFraction: rightFraction;
				topOffset: topOffset;
				rightOffset: rightOffset).
	showOnlySource ifFalse: [
		self		
			addMorph: self joinMorph
			fullFrame: (LayoutFrame identity
				leftFraction: 0.5;
				rightFraction: 0.5;
				leftOffset: halfWidth negated;
				rightOffset: halfWidth;
				topOffset: topOffset).
		] ].
	showOnlySource ifFalse: [
		self
			addMorph: self dstMorph
			fullFrame: (LayoutFrame identity
				leftFraction: leftFraction;
				topOffset: topOffset;
				leftOffset: leftOffset;
				rightOffset: (self scrollbarMorph width + self mapMorph width) negated) ].
				
	self				
		addMorph: self scrollbarMorph
		fullFrame: (LayoutFrame identity
			leftFraction: 1;
			leftOffset: self scrollbarMorph width negated - self mapMorph width;
			rightOffset: self mapMorph width negated;
			topOffset: topOffset).
	self					
		addMorph: self mapMorph
		fullFrame: (LayoutFrame identity
			leftFraction: 1;
			leftOffset: self mapMorph width negated;
			topOffset: topOffset).

]

{ #category : #initialization }
DiffMorph >> addMorphsWithOptions [

	self createOptionsPanel.

	self 
		addMainMorphsWith: optionsPanel height.

	self
		addMorph: optionsPanel 
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			bottomOffset: optionsPanel height)
]

{ #category : #initialization }
DiffMorph >> addMorphsWithoutOptions [
	self addMainMorphsWith: 0
]

{ #category : #protocol }
DiffMorph >> addOptionsPanel [

	showOptions := true.
	self removeAllMorphs.
	self addMorphsWithOptions
]

{ #category : #initialization }
DiffMorph >> addTitlePanel: initialOffset [

	"Add the title panel and returns the top offset for the rest of the window."

	| titlePanel titleRow rightLabel |
	(leftCaption isNotNil and: [ rightCaption isNotNil ]) ifFalse: [ ^ initialOffset ].
	
	"If we are showing only one panel there is no need for title"
	showOnlyDestination ifTrue: [ ^ initialOffset ].
	showOnlySource ifTrue: [ ^ initialOffset ].
	
	titlePanel := self newPanel. 	
	
	titleRow := self newPanel.
	
	titleRow changeProportionalLayout.
	titleRow hResizing: #spaceFill.
	titleRow vResizing: #shrinkWrap.	
	
	rightLabel := (self newRow: { self newLabel: rightCaption }) listCentering: #bottomRight; yourself.
	titleRow addMorph: rightLabel fullFrame: (LayoutFrame identity leftFraction: 0.5; rightOffset: mapMorph width negated - 5). 
	titleRow addMorph: (self newLabel: leftCaption) fullFrame: (LayoutFrame identity leftFraction: 0; rightFraction: 0.5; leftOffset: 10). 

		
	titlePanel addMorph: titleRow.
	titleRow extent: titlePanel minExtent.

	titlePanel vResizing: #shrinkWrap.
	titlePanel hResizing: #spaceFill.
				
	self addMorph: titlePanel
		fullFrame:(LayoutFrame identity
			topOffset: initialOffset;
			bottomFraction: 0;
			bottomOffset: titlePanel height + initialOffset).

	titlePanel extent: titlePanel minExtent.	
				
	^ initialOffset + titlePanel height.
				

]

{ #category : #'accessing colors' }
DiffMorph >> additionColor [
	"Answer the color used to show additions."

	^ self theme diffAdditionalLineBackgroundColor 
]

{ #category : #'accessing colors' }
DiffMorph >> additionHighlightColor [
	"Answer the color used to show addition highlights."
	^ self theme diffAdditionalTextInLineBackgroundColor
]

{ #category : #accessing }
DiffMorph >> adoptPaneColor: paneColor [
	"Change our border color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderStyle baseColor: paneColor
]

{ #category : #actions }
DiffMorph >> applyHighlights [
	"Apply the relevant highlights to src and dst."

	self srcMorph highlights: (self joinMappings flatCollect: [:j | j src highlights]).
	self dstMorph highlights: (self joinMappings flatCollect: [:j | j dst highlights])
]

{ #category : #actions }
DiffMorph >> applyJoin [
	"Apply the join mappings to the join morph."

	self joinMorph mappings: self joinMappings
]

{ #category : #actions }
DiffMorph >> applyMap [
	"Apply the join mappings to the map morph."

	self mapMorph mappings: self joinMappings
]

{ #category : #actions }
DiffMorph >> applyPrettyPrinter [
	"Apply pretty printer if check box is on"

	(self prettyPrint and: [ self contextClass notNil ])
		ifTrue: [ 
			self sourceTextModel getString isEmpty
				ifFalse: [ self sourceTextModel formatSourceCodeInView ].
			self destTextModel getString isEmpty
				ifFalse: [ self destTextModel formatSourceCodeInView ] ]
]

{ #category : #actions }
DiffMorph >> calculateDifference [
	"Calculate the difference of the src and dst.
	Use src/dest morphs text, because we may want to compare the pretty printed text"

	self difference: ((TextDiffBuilder
		from: self srcMorph getText to: self dstMorph getText)
			buildPatchSequence)
]

{ #category : #actions }
DiffMorph >> calculateJoinMappings [
	"Calculate the join parameters between src and dst
	and store in joinMappings."

	self joinMappings: self calculatedJoinMappings
]

{ #category : #actions }
DiffMorph >> calculatedJoinMappings [
	"Calculate the join parameters between src and dst and answer"

	| sourceLine destinationLine joins destinationRunStart sourceRunStart destinationRunEnd sourceRunEnd matchDestinationStart matchSourceStart |
	sourceLine := destinationLine := 0.
	joins := OrderedCollection new.
	destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := matchSourceStart := matchDestinationStart := 0.
	self difference
		do: [ :p | 
			p key = #match
				ifTrue: [ 
					sourceLine := sourceLine + 1.
					destinationLine := destinationLine + 1.
					matchSourceStart = 0
						ifTrue: [ 
							matchSourceStart := sourceLine.
							matchDestinationStart := destinationLine ].
					(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
						ifTrue: [ 
							sourceRunStart = 0
								ifTrue: [ sourceRunStart := sourceLine ].
							destinationRunStart = 0
								ifTrue: [ destinationRunStart := destinationLine ].
							sourceRunEnd = 0
								ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
							destinationRunEnd = 0
								ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
							joins
								add:
									(self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := 0 ] ].
			p key = #remove
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].
					sourceLine := sourceLine + 1.
					sourceRunStart = 0
						ifTrue: [ sourceRunStart := sourceLine ].
					sourceRunEnd := sourceLine ].
			p key = #insert
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].
					destinationLine := destinationLine + 1.
					sourceRunStart > 0
						ifTrue: [ 
							sourceRunEnd = 0
								ifTrue: [ sourceRunEnd := sourceRunStart ].
							destinationRunEnd = 0
								ifTrue: [ destinationRunEnd := destinationRunStart ].
							joins
								add:
									(self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := 0 ].
					destinationRunStart = 0
						ifTrue: [ destinationRunStart := destinationLine ].
					destinationRunEnd := destinationLine ] ].
	sourceLine := sourceLine + 1.
	destinationLine := destinationLine + 1.
	(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
		ifTrue: [ 
			sourceRunStart = 0
				ifTrue: [ sourceRunStart := sourceLine ].
			destinationRunStart = 0
				ifTrue: [ destinationRunStart := destinationLine ].
			sourceRunEnd = 0
				ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
			destinationRunEnd = 0
				ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
			joins
				add: (self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)) ].
	matchSourceStart > 0
		ifTrue: [ 
			joins
				add:
					(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine - 1) to: (matchDestinationStart to: destinationLine - 1)) ].
	^ joins
]

{ #category : #actions }
DiffMorph >> calibrateScrollbar [
	"Set the scrollbar parameters to match the texts."

	| maxY range delta innerH |
	self fullBounds.
	maxY := self srcMorph textArea extent y max: self dstMorph extent y.
	innerH := self dstMorph innerBounds extent y.
	delta := 91.	"self dstMorph textMorph defaultLineHeight"
	range := maxY - innerH max: 0.
	range = 0
		ifTrue: [ 
			^ self scrollbarMorph
				scrollDelta: 0.02 pageDelta: 0.2;
				interval: 1.0;
				setValue: 0.0 ].
	self scrollbarMorph
		scrollDelta: (delta / range) asFloat pageDelta: ((innerH - delta) / range) asFloat;
		interval: (innerH / maxY) asFloat;
		setValue:
			((self srcMorph scrollPane scroller offset y max: self dstMorph scrollPane scroller offset y) / range
				min: 1.0) asFloat
]

{ #category : #'accessing colors' }
DiffMorph >> colorForType: type [
	"Anwser the color to use for the given change type."

	^ {self matchColor. self additionColor. self removalColor. self modificationColor}
		at: (#(match addition removal modification) indexOf: type)
]

{ #category : #accessing }
DiffMorph >> contextClass [
	"Answer the value of contextClass"

	^ contextClass 
]

{ #category : #accessing }
DiffMorph >> contextClass: aClass [
	"Set the value of contextClass"

	contextClass := aClass
]

{ #category : #initialization }
DiffMorph >> createOptionsPanel [

	| ppCheckbox |
	ppCheckbox := self newPrettyPrintCheckboxMorph.

	optionsPanel := self newPanel
		addMorph: ((self newRow: {ppCheckbox}) listCentering: #bottomRight);
		color: self defaultColor.
		
	optionsPanel vResizing: #shrinkWrap.
	optionsPanel extent: optionsPanel minExtent.

]

{ #category : #accessing }
DiffMorph >> defaultTitle [
	"Answer the default title label for the receiver."

	^'Diff' translated
]

{ #category : #'instance creation' }
DiffMorph >> destTextModel [
	^ destTextModel ifNil: [ destTextModel := RubScrolledTextModel new interactionModel: self ].
]

{ #category : #'accessing colors' }
DiffMorph >> diffMapColor [
	"Answer the color used for the mapping bar on the left side."
	^ self theme diffMapColor
]

{ #category : #accessing }
DiffMorph >> difference [
	"Answer the value of difference"

	^ difference
]

{ #category : #accessing }
DiffMorph >> difference: anObject [
	"Set the value of difference"

	difference := anObject
]

{ #category : #accessing }
DiffMorph >> doItContext [

	^ nil
]

{ #category : #accessing }
DiffMorph >> doItReceiver [
	^nil
]

{ #category : #accessing }
DiffMorph >> dstMorph [
	"Answer the value of dstMorph"

	^ dstMorph
]

{ #category : #accessing }
DiffMorph >> dstMorph: anObject [
	"Set the value of dstMorph"

	dstMorph := anObject
]

{ #category : #'accessing colors' }
DiffMorph >> edgeColor [
	"Answer the color used to show the border of the changes."
	^ self theme diffEdgeColor
]

{ #category : #'instance creation' }
DiffMorph >> from: old to: new [
	"Set the old (src) and new (dst) text."

	self sourceTextModel setText: old.
	self destTextModel setText: new.
	self
		applyPrettyPrinter;
		calculateDifference;
		calculateJoinMappings;
		calibrateScrollbar;
		applyHighlights;
		applyJoin;
		applyMap
]

{ #category : #'instance creation' }
DiffMorph >> from: old to: new contextClass: aClass [
	"Set the old (src) and new (dst) text."

	self contextClass: aClass.
	self sourceTextModel setText: old.
	self destTextModel setText: new.
	self
		applyPrettyPrinter;
		calculateDifference;
		calculateJoinMappings;
		calibrateScrollbar;
		applyHighlights;
		applyJoin;
		applyMap
]

{ #category : #private }
DiffMorph >> hideOrShowScrollBar [
	"Do nothing"
	
]

{ #category : #'event handling' }
DiffMorph >> horizontalDestScroll: anAnnouncement [
	self srcMorph scrollPane hScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue
]

{ #category : #'event handling' }
DiffMorph >> horizontalSourceScroll: anAnnouncement [
	self dstMorph scrollPane hScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue
]

{ #category : #'user interface' }
DiffMorph >> initialExtent [
	"Answer the initial extent for the receiver."

	^RealEstateAgent standardWindowExtent
]

{ #category : #initialization }
DiffMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	showOnlyDestination := false.
	showOnlySource := false.
	showOptions := true.
	self
		srcMorph: self newSrcMorph;
		joinMorph: self newJoinMorph;
		dstMorph: self newDstMorph;
		scrollbarMorph: self newScrollbarMorph;
		mapMorph: self newMapMorph;
		changeProportionalLayout.
	self prettyPrint: TextDiffBuilder diffsWithPrettyPrint.
	self addMorphsWithOptions.
	self srcMorph scrollPane announcer 
		when: RubVerticalScrolled send: #verticalScroll: to: self;
		when: RubHorizontalScrolled send: #horizontalSourceScroll: to: self.
	self dstMorph scrollPane announcer 
		when: RubVerticalScrolled send: #verticalScroll: to: self;
		when: RubHorizontalScrolled send: #horizontalDestScroll: to: self.
	self
		linkSubmorphsToSplitters;
		extent: self initialExtent
]

{ #category : #actions }
DiffMorph >> join: aJoin selected: aBoolean [
	"Set the selection for the given join and update the
		src dst and join morphs."

	aJoin selected: aBoolean.
	self srcMorph changed.
	self joinMorph changed.
	self dstMorph changed
]

{ #category : #'accessing colors' }
DiffMorph >> joinColor [
	"Answer the color used for the join bar."
	^ self theme diffJoinColor
]

{ #category : #accessing }
DiffMorph >> joinMappings [
	"Answer the join parameters between src and dst."

	^joinMappings ifNil: [self calculateJoinMappings]
]

{ #category : #accessing }
DiffMorph >> joinMappings: aCollection [
	"Set the join parameters between src and dst."

	joinMappings := aCollection
]

{ #category : #accessing }
DiffMorph >> joinMorph [
	"Answer the value of joinMorph"

	^ joinMorph
]

{ #category : #accessing }
DiffMorph >> joinMorph: anObject [
	"Set the value of joinMorph"

	joinMorph := anObject
]

{ #category : #actions }
DiffMorph >> joinSectionClass [
	"Answer the class to use for a new join section."

	^JoinSection
]

{ #category : #accessing }
DiffMorph >> leftLabel: aLeftLabel rightLabel: aRigthLabel [

	leftCaption := aLeftLabel.
	rightCaption := aRigthLabel.
	
	self updateMorphs
]

{ #category : #'event handling' }
DiffMorph >> mapClicked: aFloat [
	"Update the scrollbar value to match a click in the map."

	self scrollbarMorph setValue: aFloat
]

{ #category : #accessing }
DiffMorph >> mapMorph [
	"Answer the value of mapMorph"

	^ mapMorph
]

{ #category : #accessing }
DiffMorph >> mapMorph: anObject [
	"Set the value of mapMorph"

	mapMorph := anObject
]

{ #category : #'accessing colors' }
DiffMorph >> matchColor [
	"Answer the color used to show matches."

	^ Color transparent
]

{ #category : #'accessing colors' }
DiffMorph >> modificationColor [
	"Answer the color used to show changes."
	^ self theme diffModificatedLineBackgroundColor
]

{ #category : #'instance creation' }
DiffMorph >> newDstMorph [
	"Answer a new dst text morph."

	^ self destTextModel newScrolledText
		vScrollbarShowNever;
		beForSmalltalkCode;
		beNotWrapped;
		beReadOnly;
		yourself
]

{ #category : #'instance creation' }
DiffMorph >> newHighlight [
	"Anewser a new highlight."

	^TextHighlightByBounds new
		color: self modificationColor;
		borderWidth: 1;
		borderColor: self edgeColor
]

{ #category : #'instance creation' }
DiffMorph >> newHighlight: type [
	"Anewser a new highlight."

	^TextHighlightByBounds new
		color: (self colorForType: type);
		borderWidth: 1;
		borderColor: self edgeColor;
		fillWidth: true
]

{ #category : #'instance creation' }
DiffMorph >> newJoinMorph [
	"Answer a new join morph."

	| w |
	w := DiffJoinMorph splitterWidth.
	^ DiffJoinMorph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		extent: w @ 4;
		minWidth: w;
		color: self joinColor
]

{ #category : #'instance creation' }
DiffMorph >> newJoinSection [
	"Answer a new join section."

	^self joinSectionClass new
		srcColor: self modificationColor;
		dstColor: self modificationColor;
		borderWidth: 1;
		borderColor: self edgeColor;
		additionHighlightColor: self additionHighlightColor;
		removalHighlightColor: self removalHighlightColor;
		addDependent: self;
		yourself
]

{ #category : #'instance creation' }
DiffMorph >> newJoinSectionFrom: srcRange to: dstRange [
	"Answer a new join section."

	|sourceParagraphLines destinationParagraphLines sourceTopPixelPosition sourceBottomPixelPosition destinationTopPixelPosition destinationBottomPixelPosition type rectangleColor|
	sourceParagraphLines := self srcMorph textMorph paragraph lines.
	destinationParagraphLines := self dstMorph textMorph paragraph lines.
	type := #modification.
	sourceTopPixelPosition := srcRange first > sourceParagraphLines size
		ifTrue: [type := #addition.
				sourceParagraphLines last bottom truncated - 1]
		ifFalse: [(sourceParagraphLines at: srcRange first) top truncated - 1].
	sourceBottomPixelPosition := srcRange size < 1
		ifTrue: [type := #addition.
				 sourceTopPixelPosition + 3]
		ifFalse: [srcRange last > sourceParagraphLines size
				ifTrue: [sourceParagraphLines last bottom truncated + 3]
				ifFalse: [(sourceParagraphLines at: srcRange last) bottom truncated - 1]].
	destinationTopPixelPosition := dstRange first > destinationParagraphLines size
		ifTrue: [type := #removal.
				destinationParagraphLines last bottom truncated - 1]
		ifFalse: [(destinationParagraphLines at: dstRange first) top truncated - 1].
	destinationBottomPixelPosition := dstRange size < 1
		ifTrue: [type := #removal.
				destinationTopPixelPosition + 3]
		ifFalse: [dstRange last > destinationParagraphLines size
				ifTrue: [destinationParagraphLines last bottom truncated + 3]
				ifFalse: [(destinationParagraphLines at: dstRange last) bottom truncated - 1]].
	rectangleColor := self colorForType: type.
	^self newJoinSection
		type: type;
		srcColor: rectangleColor;
		dstColor: rectangleColor;
		srcLineRange: srcRange;
		dstLineRange: dstRange;
		srcRange: (sourceTopPixelPosition to: sourceBottomPixelPosition);
		dstRange: (destinationTopPixelPosition to: destinationBottomPixelPosition);
		createHighlightsFrom: self srcMorph textMorph paragraph
		to: self dstMorph textMorph paragraph
]

{ #category : #'instance creation' }
DiffMorph >> newMapMorph [
	"Answer a new map morph."

	^(DiffMapMorph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		extent: 20@4;
		minWidth: 20;
		borderStyle: (BorderStyle inset width: 1))
			when: #mapClicked
			send: #mapClicked:
			to: self
]

{ #category : #'instance creation' }
DiffMorph >> newMatchJoinSectionFrom: srcRange to: dstRange [
	"Answer a new match join section."

	| spl dpl sy1 sy2 dy1 dy2 c |
	spl :=self srcMorph textMorph paragraph lines.
	dpl := self dstMorph textMorph paragraph lines.
	sy1 := (spl at: srcRange first) top truncated.
	sy2 := (spl at: srcRange last) bottom truncated.
	dy1 := (dpl at: dstRange first) top truncated.
	dy2 := (dpl at: dstRange last) bottom truncated.
	c := self colorForType: #match.
	^ self newJoinSection
		type: #match;
		borderWidth: 0;
		srcColor: c;
		dstColor: c;
		srcLineRange: srcRange;
		dstLineRange: dstRange;
		srcRange: (sy1 to: sy2);
		dstRange: (dy1 to: dy2);
		createHighlightsFrom: self srcMorph textMorph paragraph to: self dstMorph textMorph paragraph
]

{ #category : #'instance creation' }
DiffMorph >> newPrettyPrintCheckboxMorph [
	"Answer a new checkbox for specifying whether to use
	pretty printing for the diff texts."

	^self
		newCheckboxFor: self
		getSelected: #prettyPrint
		setSelected: #prettyPrint:
		getEnabled: nil
		label: 'Pretty print' translated
		help: 'If selected, pretty print will be applied to any displayed method source (eliminates trivial formatting changes)' translated
]

{ #category : #'instance creation' }
DiffMorph >> newScrollbarMorph [
	"Answer a new scrollbar morph."

	^ RubScrollBar new
		model: self;
		setValueSelector: #vScroll:;
		vResizing: #spaceFill;
		width: self theme scrollbarThickness
]

{ #category : #'instance creation' }
DiffMorph >> newSrcMorph [
	"Answer a new src text morph."

	^ self sourceTextModel newScrolledText
		vScrollbarShowNever;
		beForSmalltalkCode;
		beNotWrapped;
		beReadOnly;
		yourself
]

{ #category : #protocol }
DiffMorph >> on: aModel [

	aModel addDependent: self
]

{ #category : #accessing }
DiffMorph >> prettyPrint [
	"Answer the value of prettyPrint"

	^ prettyPrint
]

{ #category : #accessing }
DiffMorph >> prettyPrint: aBoolean [
	"Set the value of prettyPrint"

	prettyPrint == aBoolean ifTrue: [^self].
	prettyPrint := aBoolean.
	self updateText
	
]

{ #category : #'accessing colors' }
DiffMorph >> removalColor [
	"Answer the color used to show removals."
	^ self theme diffRemovedLinesBackgroundColor
]

{ #category : #'accessing colors' }
DiffMorph >> removalHighlightColor [
	"Answer the color used to show removal highligths."
	^ self theme diffRemovalHighlightColor
]

{ #category : #protocol }
DiffMorph >> removeOptionsPanel [

	showOptions := false.
	self removeAllMorphs.
	self addMorphsWithoutOptions
]

{ #category : #accessing }
DiffMorph >> scrollbarMorph [
	"Answer the value of scrollbarMorph"

	^ scrollbarMorph
]

{ #category : #accessing }
DiffMorph >> scrollbarMorph: anObject [
	"Set the value of scrollbarMorph"

	scrollbarMorph := anObject
]

{ #category : #accessing }
DiffMorph >> selectedClassOrMetaClass [
	^ self contextClass 
]

{ #category : #private }
DiffMorph >> setOptionsPanel: aBoolean [
	"This is a private method. The implementation is based on the fact the options are set by default"
	
	aBoolean ifFalse: [ self removeOptionsPanel ]
]

{ #category : #'user interface' }
DiffMorph >> shoutAboutToStyle: aPluggableShoutMorphOrView [
	aPluggableShoutMorphOrView classOrMetaClass: self contextClass.
	^ self contextClass notNil 
]

{ #category : #accessing }
DiffMorph >> showBoth [

	showOnlyDestination ifTrue: [ showOnlyDestination := false ].
	showOnlySource ifTrue: [ showOnlySource := false ].
	
	self updateMorphs
]

{ #category : #accessing }
DiffMorph >> showOnlyDestination [

	^ showOnlyDestination
]

{ #category : #accessing }
DiffMorph >> showOnlyDestination: aBoolean [

	showOnlyDestination = aBoolean ifTrue: [ ^ self ].
	self showOnlySource: false.
	showOnlyDestination := aBoolean.
	self updateMorphs
]

{ #category : #accessing }
DiffMorph >> showOnlySource [

	^ showOnlySource
]

{ #category : #accessing }
DiffMorph >> showOnlySource: aBoolean [

	showOnlySource = aBoolean ifTrue: [ ^ self ].
	self showOnlyDestination: false.
	showOnlySource := aBoolean.
	self updateMorphs
]

{ #category : #accessing }
DiffMorph >> showOptions [

	^ showOptions
]

{ #category : #accessing }
DiffMorph >> showOptions: aBoolean [

	showOptions = aBoolean ifTrue: [ ^ self ].
	showOptions := aBoolean.
	aBoolean
		ifTrue: [ self addOptionsPanel ]
		ifFalse: [ self removeOptionsPanel ]
]

{ #category : #'instance creation' }
DiffMorph >> sourceTextModel [
	^ sourceTextModel ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ].
]

{ #category : #accessing }
DiffMorph >> srcMorph [
	"Answer the value of srcMorph"

	^ srcMorph
]

{ #category : #accessing }
DiffMorph >> srcMorph: anObject [
	"Set the value of srcMorph"

	srcMorph := anObject
]

{ #category : #updating }
DiffMorph >> themeChanged [
	"Update the scrollbar width/frame."

	|offset|
	super themeChanged.
	self scrollbarMorph width: self theme scrollbarThickness.
	offset := self scrollbarMorph width negated - self mapMorph width.
	self scrollbarMorph layoutFrame leftOffset: offset.
	self dstMorph layoutFrame rightOffset: offset
]

{ #category : #updating }
DiffMorph >> update: aSymbol [

	aSymbol == #addOptions
		ifTrue: [ ^ self addOptionsPanel ].
	aSymbol == #removeOptions	
		ifTrue: [ ^ self removeOptionsPanel ].
		
	^ super update: aSymbol
]

{ #category : #updating }
DiffMorph >> update: aSymbol with: aValue [

	aSymbol == #showOnlyDestination
		ifTrue: [ ^ self showOnlyDestination: aValue ].
	aSymbol == #showOptions	
		ifTrue: [ ^ self showOptions: aValue ].
		
	^ super update: aSymbol with: aValue
]

{ #category : #updating }
DiffMorph >> updateJoinOffsets [
	"Update the src and dst offsets in the join morph
	to match the src and dst tex scroll offsets."

	self joinMorph
		srcOffset: 0 @ self srcMorph scrollPane scroller offset y negated;
		dstOffset: 0 @ self dstMorph scrollPane scroller offset y negated;
		changed
]

{ #category : #protocol }
DiffMorph >> updateMorphs [

	self removeAllMorphs.
	
	showOptions
		ifTrue: [ self addMorphsWithOptions ]
		ifFalse: [ self addMorphsWithoutOptions ]
]

{ #category : #'user interface' }
DiffMorph >> updateText [
	"Reset the text if we have some."

	(self sourceTextModel getString notNil and: [ self destTextModel getString notNil ])
		ifTrue: [ self from: self sourceTextModel getString to: self destTextModel getString ]
]

{ #category : #updating }
DiffMorph >> vScroll: scrollValue [
	"Called from standalone scroolbar.
	Scroll the srcMorph and redo the join."

	self srcMorph scrollPane vScrollbarValue: scrollValue.
	self dstMorph scrollPane vScrollbarValue: scrollValue.
	self updateJoinOffsets
]

{ #category : #'event handling' }
DiffMorph >> verticalScroll: anAnnouncement [
	"Called from src when scrolled by keyboard etc.."

	self srcMorph scrollPane vScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue.
	self dstMorph scrollPane vScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue.
	self scrollbarMorph value: anAnnouncement scrollValue.
	self updateJoinOffsets
]
