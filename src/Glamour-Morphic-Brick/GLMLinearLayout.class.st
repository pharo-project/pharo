Class {
	#name : #GLMLinearLayout,
	#superclass : #LayoutPolicy,
	#category : #'Glamour-Morphic-Brick-Layouts'
}

{ #category : #layout }
GLMLinearLayout >> align: aBrick in: parentBrick anchorPoint: aPoint inContext: aContext [

	^ self subclassResponsibility
]

{ #category : #floating }
GLMLinearLayout >> floatingMethodFor: aSymbol [

	^ ('layoutFloating', (aSymbol capitalized), ':in:') asSymbol
]

{ #category : #testing }
GLMLinearLayout >> hasHeightRestrictions: aBrick [

	^ aBrick layout hasMinHeight | aBrick layout hasMaxHeight
]

{ #category : #testing }
GLMLinearLayout >> hasWidthRestrictions: aBrick [
	^ aBrick layout hasMinWidth or: [ aBrick layout hasMaxWidth ]
]

{ #category : #height }
GLMLinearLayout >> heightChildrenDependency: aBrick [

	^ self subclassResponsibility
]

{ #category : #height }
GLMLinearLayout >> heightMyselfDependency: aBrick [
	"when height depends only from aBrick just return its cutom height"
	
	^ aBrick layout height brickValue: aBrick brick
]

{ #category : #height }
GLMLinearLayout >> heightParentDependency: aBrick [

	^ self subclassResponsibility
]

{ #category : #floating }
GLMLinearLayout >> isFloating: aBrick [
	
	aBrick layout floating = #none ifTrue: [ ^ false ].
	
	^ self respondsTo: (self floatingMethodFor: aBrick layout floating)
]

{ #category : #layout }
GLMLinearLayout >> layout: aBrick in: newBounds [

	^ self subclassResponsibility
]

{ #category : #layout }
GLMLinearLayout >> layout: aBrick in: aRectangle inject: anInteger into: aBlock [

	aBrick subbricks inject: anInteger into:[ :origin :each | |result|
		result := origin + ((self isFloating: each)
			ifFalse: [ aBlock cull: origin cull: each ]
			ifTrue: [ self layoutFloating: each in: aRectangle ]).
		"applying aligning: #none, #left, #center, #right"
		self align: each in: aBrick anchorPoint: origin inContext: nil.
		each onLayouted.
		result ].
]

{ #category : #floating }
GLMLinearLayout >> layoutFloating: aBrick in: newBounds [
	"must return an integer that will be added to the offset, if necessary"
	
	|float symbol|
	float := aBrick layout floating.
	symbol := self floatingMethodFor: float.
	
	^ (self respondsTo: symbol)
		ifTrue: [ self perform: symbol with: aBrick with: newBounds ]
		ifFalse: [ 0 ]
]

{ #category : #floating }
GLMLinearLayout >> layoutFloatingLeft: aBrick in: newBounds [
	| left top |
	
	left := aBrick marginLeft.
	top := aBrick marginTop.
	
	aBrick brickBounds: (left@top extent: aBrick extent).
	
	^ 0
]

{ #category : #floating }
GLMLinearLayout >> layoutFloatingRight: aBrick in: newBounds [
	| left top |
	
	left := aBrick owner width - aBrick width - aBrick marginRight - aBrick owner paddingRight.
	top := aBrick marginTop + aBrick owner paddingTop.
	
	aBrick brickBounds: (left@top extent: aBrick extent).
	
	^ 0
]

{ #category : #enumerating }
GLMLinearLayout >> nonFloatSubbricks: aBrick [

	^ aBrick subbricks select: [ :each | each layout floating = #none ]
]

{ #category : #height }
GLMLinearLayout >> normalizeHeight: anInteger of: aBrick [

	^ (anInteger max: (aBrick layout minHeight brickValue: aBrick))
		min: (aBrick layout maxHeight brickValue: aBrick)
]

{ #category : #width }
GLMLinearLayout >> normalizeWidth: anInteger of: aBrick [

	^ (anInteger max: (aBrick layout minWidth brickValue: aBrick))
		min: (aBrick layout maxWidth brickValue: aBrick)
]

{ #category : #notifying }
GLMLinearLayout >> sendEvent: aLayoutEvent to: aBrick [

	aBrick onLayoutEvent: aLayoutEvent
]

{ #category : #height }
GLMLinearLayout >> updateHeight: aBrick [
	"this method should be called only when height of aBrick is dirty"
	aBrick isHeightClean ifTrue: [ Exception signal: 'Clean height can''t be updated!' ].
	aBrick layout vState updateHeight: aBrick on: self
	
	
	
]

{ #category : #width }
GLMLinearLayout >> updateWidth: aBrick [
	"aBrick is a Brick that uses me as current layout"
	"this method should be called only when width of aBrick is dirty"
	aBrick isWidthClean ifTrue: [ Exception signal: 'Clean width can''t be updated!' ].
	aBrick layout hState updateWidth: aBrick on: self
	
	
	
]

{ #category : #width }
GLMLinearLayout >> widthChildrenDependency: aBrick [

	^ self subclassResponsibility
]

{ #category : #width }
GLMLinearLayout >> widthMyselfDependency: aBrick [

	^ aBrick layout width brickValue: aBrick brick
]

{ #category : #width }
GLMLinearLayout >> widthParentDependency: aBrick [
	
	^ self subclassResponsibility
]
