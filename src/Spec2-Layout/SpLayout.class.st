"
A SpecPresenter is a object used to describe a user interface
"
Class {
	#name : #SpLayout,
	#superclass : #SpLegacyLayout,
	#instVars : [
		'type',
		'commands',
		'currentOffset',
		'currentProportional',
		'numberOfProportionals',
		'lastFractionIndex',
		'autoFractionComputation',
		'size',
		'selector'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpLayout class >> composed [

	^self new
		type: #SpContainerPresenter;
		yourself
]

{ #category : #constants }
SpLayout class >> windowBorderWidth [

	^ 0
]

{ #category : #commands }
SpLayout >> add: aSpec [ 
	
	self add: aSpec origin: 0@0 corner: 1@1
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock bottom: aNumber [
	
	self add: aBlock top: nil bottom: aNumber left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock left: aNumber [
	
	self add: aBlock top: nil bottom: nil left: aNumber right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock left: left right: right [
	
	self add: aBlock top: nil bottom: nil left: left right: right
]

{ #category : #commands }
SpLayout >> add: aSpec origin: originPoint corner: cornerPoint [ 

	self add: aSpec origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpLayout >> add: aSpec origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	
	commands add: (SpLayoutAdd 
							subwidget: aSpec
							layoutFrame: 
								((originPoint corner: cornerPoint) asSpLayoutFrame 
									topLeftOffset: ooPoint;
									bottomRightOffset: ocPoint ))
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock right: aNumber [
	
	self add: aBlock top: nil bottom: nil left: nil right: aNumber
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock top: aNumber [

	self add: aBlock top: aNumber bottom: nil left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock top: top bottom: bottom [
	
	self add: aBlock top: top bottom: bottom left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock top: top bottom: bottom left: left right: right [
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: left right: right.
		
	self 
		add: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth
]

{ #category : #commands }
SpLayout >> add: modelGetter withSpec: aSymbol [ 
	
	self add: modelGetter withSpec: aSymbol origin: 0@0 corner: 1@1
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec bottom: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: aNumber left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec left: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: aNumber right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec left: left right: right [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: left right: right
]

{ #category : #commands }
SpLayout >> add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint [ 

	self add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpLayout >> add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [

	commands add: (SpLayoutAddWithSpec 
							subwidget: modelGetter 
							spec: aSymbol 
							layoutFrame: ((originPoint corner: cornerPoint) asSpLayoutFrame 
												topLeftOffset: ooPoint;
												bottomRightOffset: ocPoint))
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec right: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: nil right: aNumber
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec top: aNumber [
	
	self add: aBlock withSpec: aSpec top: aNumber bottom: nil left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec top: top bottom: bottom [
	
	self add: aBlock withSpec: aSpec top: top bottom: bottom left: nil right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> add: aBlock withSpec: aSpec top: top bottom: bottom left: left right: right [
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: left right: right.
		
	self 
		add: aBlock
		withSpec: aSpec 
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth
]

{ #category : #converting }
SpLayout >> addHSplitter [

	self commands add: (SpSplitterHorizontal commands: self commands)
]

{ #category : #commands }
SpLayout >> addSpacer [
	self add: #specSpacer
]

{ #category : #converting }
SpLayout >> addVSplitter [

	self commands add: (SpSplitterVertical commands: self commands)
]

{ #category : #converting }
SpLayout >> asArray [
	^ (OrderedCollection new
		add: self type;
		addAll: self privateAsArray;
		yourself) asArray
]

{ #category : #building }
SpLayout >> asSpLayout [

	^ self
]

{ #category : #accessing }
SpLayout >> commands [
	^ commands
]

{ #category : #accessing }
SpLayout >> commands: anObject [
	commands := anObject
]

{ #category : #private }
SpLayout >> computeLayoutFromTop: top bottom: bottom left: left right: right [
	| originY offsetCornerX cornerX offsetOriginY offsetCornerY cornerY originX offsetOriginX |
	
	originX := left isFloat
		ifTrue: [ left ]
		ifFalse: [ 0 ].
	originY := top isFloat
		ifTrue: [ top ]
		ifFalse: [ 0 ].
	cornerX := right isFloat
		ifTrue: [ 1 - right ]
		ifFalse: [ 1 ].
	cornerY := bottom isFloat
		ifTrue: [ 1 - bottom ]
		ifFalse: [ 1 ].
	offsetOriginX := left isInteger
		ifTrue: [ left ]
		ifFalse: [ 0 ].
	offsetOriginY := top isInteger
		ifTrue: [ top ]
		ifFalse: [ 0 ].
	offsetCornerX := right isInteger
		ifTrue: [ right negated ]
		ifFalse: [ 0 ].
	offsetCornerY := bottom isInteger
		ifTrue: [ bottom negated ]
		ifFalse: [ 0 ].
		
	^ { originX@originY. cornerX@cornerY. offsetOriginX@offsetOriginY. offsetCornerX@offsetCornerY }
]

{ #category : #private }
SpLayout >> computeNonSplitterWidgetsIn: result [
	(self commands reject: #isSplitter)
		do: [ :widget | 
			self
				setOffsetsFor: widget
				top: widget topOffset
				left: widget leftOffset
				bottom: widget bottomOffset
				right: widget rightOffset
				borderWidth: self class windowBorderWidth.
			result addAll: widget asSpecElements ]
]

{ #category : #private }
SpLayout >> computeSplittersIn: result [
	(self commands select: #isSplitter)
		ifNotEmpty: [ :spliters | 
			spliters do: [ :spliter | result addAll: spliter asSpecElements ].
			result add: #checkSplitters ]
]

{ #category : #initialization }
SpLayout >> initialize [ 
	super initialize.
	
	type := #SpContainerPresenter.
	self commands: OrderedCollection new.
]

{ #category : #testing }
SpLayout >> isExecutable [
	"If the spec layout is executable, the interpreter will try to execute it directly (without 
	 interpreting it)"

	^ false
]

{ #category : #commands }
SpLayout >> newColumn: aBlock [ 
	
	self newColumn: aBlock origin: 0@0 corner: 1@1
]

{ #category : #'commands-advanced' }
SpLayout >> newColumn: aBlock left: aNumber [
	
	self newColumn: aBlock left: aNumber right: nil
]

{ #category : #'commands-advanced' }
SpLayout >> newColumn: aBlock left: left right: right [
	| layout |
	
	layout := self computeLayoutFromTop: nil bottom: nil left: left right: right.

	self 
		newColumn: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth
]

{ #category : #commands }
SpLayout >> newColumn: aBlock origin: originPoint corner: cornerPoint [ 

	self newColumn: aBlock origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpLayout >> newColumn: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:c | c add: aBlock ] ].
	
	commands add: (SpLayoutAddColumn
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asSpLayoutFrame
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).
]

{ #category : #'commands-advanced' }
SpLayout >> newColumn: aBlock right: aNumber [
	
	self newColumn: aBlock left: nil right: aNumber
]

{ #category : #commands }
SpLayout >> newColumn: aBlock  width: aNumber [
	
	self newColumn: aBlock  origin: 0@0 corner: 0@1 offsetOrigin: 0@0 offsetCorner: aNumber @0
]

{ #category : #commands }
SpLayout >> newRow: aBlock [ 
	
	self newRow: aBlock origin: 0@0 corner: 1@1
]

{ #category : #'commands-advanced' }
SpLayout >> newRow: aBlock bottom: aNumber [
	
	self newRow: aBlock top: nil bottom: aNumber
]

{ #category : #commands }
SpLayout >> newRow: aBlock height: aNumber [
	
	self newRow: aBlock origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@aNumber 
]

{ #category : #commands }
SpLayout >> newRow: aBlock origin: originPoint corner: cornerPoint [ 

	self newRow: aBlock origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpLayout >> newRow: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:r | r add: aBlock ] ].
	
	commands add: (SpLayoutAddRow
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asSpLayoutFrame 
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).
]

{ #category : #'commands-advanced' }
SpLayout >> newRow: aBlock top: aNumber [
	
	self newRow: aBlock top: aNumber bottom: nil
]

{ #category : #'commands-advanced' }
SpLayout >> newRow: aBlock top: top bottom: bottom [
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: nil right: nil.
		
	self 
		newRow: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth
]

{ #category : #converting }
SpLayout >> privateAsArray [
	| result |
	result := OrderedCollection new.
	
	self resetArrayComputation.
	
	self computeNonSplitterWidgetsIn: result.
	self computeSplittersIn: result.
	
	^ result asArray
]

{ #category : #converting }
SpLayout >> resetArrayComputation [
	"Do nothing here"
]

{ #category : #accessing }
SpLayout >> selector [
	^ selector
]

{ #category : #accessing }
SpLayout >> selector: aSymbol [
	selector := aSymbol
]

{ #category : #commands }
SpLayout >> send: aSelector [
	
	self send: aSelector withArguments: #()
]

{ #category : #commands }
SpLayout >> send: aSelector withArguments: arguments [
	self commands add: (SpLayoutSend selector: aSelector)
]

{ #category : #initialization }
SpLayout >> setBottomOffset: bottom for: widget borderWidth: windowBorderWidth [

	widget bottomFraction = 1
		ifTrue: [ 
			bottom isZero
				ifTrue: [ widget bottomOffset: windowBorderWidth negated ]
				ifFalse: [ widget bottomOffset: bottom - self splitterWidth ] ]
]

{ #category : #initialization }
SpLayout >> setLeftOffset: left for: widget borderWidth: windowBorderWidth [

	widget leftFraction = 0
		ifTrue: [ 
			left isZero
				ifTrue: [ widget leftOffset: windowBorderWidth ]
				ifFalse: [ widget leftOffset: left + self splitterWidth ] ]
		ifFalse: [ 
			widget leftFraction = 1
				ifFalse: [ widget leftOffset: left + self splitterWidth ] ]
]

{ #category : #private }
SpLayout >> setOffsetsFor: widget top: top left: left bottom: bottom right: right borderWidth: windowBorderWidth [

	self setRightOffset: right for: windowBorderWidth borderWidth: widget.
	self setLeftOffset: left for: widget borderWidth: windowBorderWidth.
	self setBottomOffset: bottom for: widget borderWidth: windowBorderWidth.
	self setTopOffset: top for: widget borderWidth: windowBorderWidth
]

{ #category : #initialization }
SpLayout >> setRightOffset: right for: windowBorderWidth borderWidth: widget [

	widget rightFraction = 1
		ifTrue: [ 
			right isZero
				ifTrue: [ widget rightOffset: windowBorderWidth negated ]
				ifFalse: [ widget rightOffset: right - self splitterWidth ] ]
]

{ #category : #initialization }
SpLayout >> setTopOffset: top for: widget borderWidth: windowBorderWidth [

	widget topFraction = 0
		ifTrue: [
			top isZero
				ifTrue: [ widget topOffset: windowBorderWidth ]
				ifFalse: [ widget topOffset: top + self splitterWidth ] ]
		ifFalse: [ 
			widget topFraction = 1
				ifFalse: [ widget topOffset: top + self splitterWidth ] ]
]

{ #category : #'accessing - defaults' }
SpLayout >> splitterWidth [
	
	^ 4
]

{ #category : #accessing }
SpLayout >> type [
	^ type
]

{ #category : #accessing }
SpLayout >> type: anObject [
	type := anObject
]
