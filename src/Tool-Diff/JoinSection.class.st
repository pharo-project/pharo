Class {
	#name : #JoinSection,
	#superclass : #Model,
	#instVars : [
		'src',
		'dst',
		'borderWidth',
		'borderColor',
		'type',
		'width',
		'shape',
		'additionHighlightColor',
		'removalHighlightColor'
	],
	#category : #'Tool-Diff'
}

{ #category : #adding }
JoinSection >> addHighlightsFrom: srcBlock to: dstBlock to: aCollection color: aColor [
	"Add the highlights required for the given character blocks
	of a paragraph. May be up to three highlights depending
	on the line spans."
	
	srcBlock textLine = dstBlock textLine
		ifTrue: [aCollection add: (TextHighlightByBounds new
				color: aColor;
				bounds: (srcBlock topLeft corner: dstBlock bottomRight))]
		ifFalse: [aCollection
					add: (TextHighlightByBounds new
							color: aColor;
							bounds: (srcBlock topLeft corner: srcBlock textLine bottomRight));
					add: (TextHighlightByBounds new
							fillWidth: true;
							color: aColor;
							bounds: (srcBlock bottomLeft corner: dstBlock topRight));
					add: (TextHighlightByBounds new
							color: aColor;
							bounds: (dstBlock textLine topLeft corner: dstBlock bottomRight))]
]

{ #category : #accessing }
JoinSection >> additionHighlightColor [
	^ additionHighlightColor
]

{ #category : #accessing }
JoinSection >> additionHighlightColor: anObject [
	additionHighlightColor := anObject.
	self updateHighlights
]

{ #category : #accessing }
JoinSection >> borderColor [
	"Answer the value of borderColor"

	^ borderColor
]

{ #category : #accessing }
JoinSection >> borderColor: aColor [
	"Set the value of borderColor"

	borderColor := aColor.
	self updateHighlights
]

{ #category : #accessing }
JoinSection >> borderColorToUse [
	"Answer the border color to use."
	
	^self borderColor
]

{ #category : #accessing }
JoinSection >> borderWidth [
	"Answer the value of borderWidth"

	^ borderWidth
]

{ #category : #accessing }
JoinSection >> borderWidth: anInteger [
	"Set the value of borderWidth"

	borderWidth := anInteger.
	self src highlight notNil ifTrue: [
		self src highlight borderWidth: anInteger].
	self dst highlight notNil ifTrue: [
		self dst highlight borderWidth: anInteger]
]

{ #category : #actions }
JoinSection >> clicked [
	"The receiver or a highlight was clicked."

	self wantsClick
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #testing }
JoinSection >> containsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	^self shape containsPoint: aPoint
]

{ #category : #actions }
JoinSection >> createHighlights [
	"Create and store the src and dst highlights."

	|s d|
	s := OrderedCollection new.
	d := OrderedCollection new.
	s add: (self newHighlight
			color: self src color;
			borderWidth: self borderWidth;
			bounds: (0@self src range first corner: 0@(self src range last + 1));
			borderSides: #(top left bottom)).
	d add: (self newHighlight
			color: self dst color;
			borderWidth: self borderWidth;
			bounds: (0@self dst range first corner: 0@(self dst range last + 1));
			borderSides: #(top right bottom)).
	self src highlights: s.
	self dst highlights: d
]

{ #category : #actions }
JoinSection >> createHighlightsFrom: srcPara to: dstPara [
	"Create and store the src and dst highlights.
	Use the given paragraphs to determine inline
	diffs."

	| s d si di srcText dstText diffs i sb eb line |
	self createHighlights.
	self src lineRange notEmpty
		ifTrue: [ 
			line := srcPara lines at: self src lineRange first.
			si := line first.
			line := srcPara lines at: self src lineRange last.
			srcText := srcPara string copyFrom: si to: line last ]
		ifFalse: [ srcText := '' ].
	self dst lineRange notEmpty
		ifTrue: [ 
			line := dstPara lines at: self dst lineRange first.
			di := line first.
			line := dstPara lines at: self dst lineRange last.
			dstText := dstPara string copyFrom: di to: line last ]
		ifFalse: [ dstText := '' ].
	self src text: srcText.
	self dst text: dstText.
	self type = #modification
		ifFalse: [ ^ self ].
	s := self src highlights.
	d := self dst highlights.
	diffs := (InlineTextDiffBuilder from: srcText to: dstText) buildPatchSequence groupByRuns: [ :e | e key ].
	diffs
		do: [ :c | 
			c first key = #match
				ifTrue: [ 
					c
						do: [ :a | 
							si := si + a value size.
							di := di + a value size ] ].
			c first key = #insert
				ifTrue: [ 
					i := di.
					c do: [ :a | di := di + a value size ].
					sb := dstPara characterBlockForIndex: i.
					eb := dstPara characterBlockForIndex: di - 1.
					self
						addHighlightsFrom: sb
						to: eb
						to: d
						color: self additionHighlightColor ].
			c first key = #remove
				ifTrue: [ 
					i := si.
					c do: [ :a | si := si + a value size ].
					sb := srcPara characterBlockForIndex: i.
					eb := srcPara characterBlockForIndex: si - 1.
					self
						addHighlightsFrom: sb
						to: eb
						to: s
						color: self removalHighlightColor ] ]
]

{ #category : #drawing }
JoinSection >> drawMapOn: aCanvas in: rect scale: scale [
	"Draw the join on the given canvas scaled into the given rectangle."
	
	self type = #match ifTrue: [^self].
	aCanvas
		frameAndFillRectangle: (rect left @ (((self dst range first max: 0) * scale) truncated + rect top)
						corner: (rect right @ ((self dst range last * scale) truncated + rect top)))
		fillColor: (self fillStyleFor: rect)
		borderWidth: 1
		borderColor: self borderColorToUse
]

{ #category : #drawing }
JoinSection >> drawOn: aCanvas [
	"Draw the join on the given canvas."
	
	|v bc|
	(self src color isTransparent and: [self dst color isTransparent])
		ifTrue: [^self].
	v := self shape vertices.
	aCanvas
		drawPolygon: v
		fillStyle: (self fillStyleFor: self shape bounds).
	(self borderWidth > 0 and: [self borderColor isTransparent not]) ifTrue: [
		bc := self borderColorToUse.
		aCanvas
			line: v first + (0@self borderWidth // 2)
			to: v second + (-1@self borderWidth // 2)
			width: self borderWidth
			color: bc;
			line: v third - (1@(self borderWidth // 2))
			to: v fourth - (0@(self borderWidth // 2))
			width: self borderWidth
			color: bc]
]

{ #category : #accessing }
JoinSection >> dst [
	"Answer the value of dst"

	^ dst
]

{ #category : #accessing }
JoinSection >> dst: anObject [
	"Set the value of dst"

	dst := anObject
]

{ #category : #accessing }
JoinSection >> dstColor: aColor [
	"Set the dst color"

	self dst color: aColor
]

{ #category : #accessing }
JoinSection >> dstLineRange: anInterval [
	"Set the dst lineRange."

	self dst lineRange: anInterval
]

{ #category : #accessing }
JoinSection >> dstOffset: aPoint [
	"Set the dst offset."

	self dst offset:  aPoint.
	self updateShape
]

{ #category : #accessing }
JoinSection >> dstRange: anInterval [
	"Set the dst range."

	self dst range: anInterval.
	self updateShape
]

{ #category : #accessing }
JoinSection >> fillStyleFor: rect [
	"Answer the fillStyle to use for the given rectangle."
	
	^self src color = self dst color
		ifTrue: [self src color]
		ifFalse: [(GradientFillStyle ramp: {0.0 -> self src color. 1.0 -> self dst color})
				direction: rect width@0;
				origin: rect topLeft]
]

{ #category : #initialization }
JoinSection >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		src: JoinSide new;
		dst: JoinSide new;
		shape: Polygon new;
		width: 0;
		borderWidth: 0;
		borderColor: Color transparent;
		type: #modification
]

{ #category : #'instance creation' }
JoinSection >> newHighlight [
	"Anwser a new highlight."

	^TextHighlightByBounds new
		borderWidth: 1;
		borderColor: self borderColor;
		fillWidth: true
]

{ #category : #accessing }
JoinSection >> removalHighlightColor [
	^ removalHighlightColor
]

{ #category : #accessing }
JoinSection >> removalHighlightColor: anObject [
	removalHighlightColor := anObject.
	self updateHighlights.
]

{ #category : #accessing }
JoinSection >> shape [
	"Answer the value of shape"

	^ shape
]

{ #category : #accessing }
JoinSection >> shape: anObject [
	"Set the value of shape"

	shape := anObject
]

{ #category : #accessing }
JoinSection >> src [
	"Answer the value of src"

	^ src
]

{ #category : #accessing }
JoinSection >> src: anObject [
	"Set the value of src"

	src := anObject
]

{ #category : #accessing }
JoinSection >> srcColor: aColor [
	"Set the src color."

	self src color: aColor
]

{ #category : #accessing }
JoinSection >> srcLineRange: anInterval [
	"Set the src lneRange."

	self src lineRange: anInterval
]

{ #category : #accessing }
JoinSection >> srcOffset: aPoint [
	"Set the src offset"

	self src offset:  aPoint.
	self updateShape
]

{ #category : #accessing }
JoinSection >> srcRange: anInterval [
	"Set the  src range"

	self src range: anInterval.
	self updateShape
]

{ #category : #accessing }
JoinSection >> type [
	"Answer the value of type"

	^ type
]

{ #category : #accessing }
JoinSection >> type: anObject [
	"Set the value of type"

	type := anObject
]

{ #category : #accessing }
JoinSection >> updateHighlights [
	"Update the highlight border colors."

	|bc|
	(self src isNil or: [self dst isNil]) ifTrue: [^self].
	bc := self borderColorToUse.
	self src highlight notNil ifTrue: [
		self src highlight borderColor: bc].
	self dst highlight notNil ifTrue: [
		self dst highlight borderColor: bc]
]

{ #category : #updating }
JoinSection >> updateShape [
	"Update the receiver's shape."

	(self src range isNil or: [self dst range isNil]) ifTrue: [^self].
	self shape: (Polygon vertices:
		{(0@ self src range first) + self src offset. (self width @ self dst range first) + self dst offset.
		(self width @ self dst range last) + self dst offset. (0@self src range last) + self src offset})
]

{ #category : #testing }
JoinSection >> wantsClick [
	"Don't if we are transparent for now."

	^(self src color isTransparent and: [self dst color isTransparent]) not
]

{ #category : #accessing }
JoinSection >> width [
	"Answer the value of width"

	^ width
]

{ #category : #accessing }
JoinSection >> width: anObject [
	"Set the value of width"

	width := anObject.
	self updateShape
]
