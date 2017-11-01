"
A SpecPresenter is a object used to describe a user interface
"
Class {
	#name : #SpecLayout,
	#superclass : #Object,
	#instVars : [
		'type',
		'commands',
		'selector',
		'result',
		'shouldCheckSplitters',
		'currentOffset',
		'currentProportional',
		'numberOfProportionals',
		'lastFractionIndex',
		'autoFractionComputation',
		'size'
	],
	#category : 'Spec-Layout'
}

{ #category : #'instance creation' }
SpecLayout class >> composed [

	^self new
		type: #ContainerPresenter;
		yourself
]

{ #category : #constants }
SpecLayout class >> windowBorderWidth [

	^ 0
]

{ #category : #commands }
SpecLayout >> add: aSpec [ 
	
	self add: aSpec origin: 0@0 corner: 1@1
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock bottom: aNumber [
	
	self add: aBlock top: nil bottom: aNumber left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock left: aNumber [
	
	self add: aBlock top: nil bottom: nil left: aNumber right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock left: left right: right [
	
	self add: aBlock top: nil bottom: nil left: left right: right
]

{ #category : #commands }
SpecLayout >> add: aSpec origin: originPoint corner: cornerPoint [ 

	self add: aSpec origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpecLayout >> add: aSpec origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	
	commands add: (SpecLayoutAdd 
							subwidget: aSpec
							layoutFrame: 
								((originPoint corner: cornerPoint) asSpecLayoutFrame 
									topLeftOffset: ooPoint;
									bottomRightOffset: ocPoint ))
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock right: aNumber [
	
	self add: aBlock top: nil bottom: nil left: nil right: aNumber
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock top: aNumber [

	self add: aBlock top: aNumber bottom: nil left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock top: top bottom: bottom [
	
	self add: aBlock top: top bottom: bottom left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock top: top bottom: bottom left: left right: right [
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
SpecLayout >> add: modelGetter withSpec: aSymbol [ 
	
	self add: modelGetter withSpec: aSymbol origin: 0@0 corner: 1@1
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec bottom: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: aNumber left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec left: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: aNumber right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec left: left right: right [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: left right: right
]

{ #category : #commands }
SpecLayout >> add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint [ 

	self add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpecLayout >> add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [

	commands add: (SpecLayoutAddWithSpec 
							subwidget: modelGetter 
							spec: aSymbol 
							layoutFrame: ((originPoint corner: cornerPoint) asSpecLayoutFrame 
												topLeftOffset: ooPoint;
												bottomRightOffset: ocPoint))
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec right: aNumber [
	
	self add: aBlock withSpec: aSpec top: nil bottom: nil left: nil right: aNumber
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec top: aNumber [
	
	self add: aBlock withSpec: aSpec top: aNumber bottom: nil left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec top: top bottom: bottom [
	
	self add: aBlock withSpec: aSpec top: top bottom: bottom left: nil right: nil
]

{ #category : #commands-advanced }
SpecLayout >> add: aBlock withSpec: aSpec top: top bottom: bottom left: left right: right [
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
SpecLayout >> addHSplitter [

	self commands add: (SpecSplitterHorizontal commands: self commands)
]

{ #category : #converting }
SpecLayout >> addVSplitter [

	self commands add: (SpecSplitterVertical commands: self commands)
]

{ #category : #converting }
SpecLayout >> asArray [
	^ (OrderedCollection new
		add: self type;
		addAll: self privateAsArray;
		yourself) asArray
]

{ #category : #accessing }
SpecLayout >> commands [
	^ commands
]

{ #category : #accessing }
SpecLayout >> commands: anObject [
	commands := anObject
]

{ #category : #private }
SpecLayout >> computeLayoutFromTop: top bottom: bottom left: left right: right [
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
SpecLayout >> computeNotSplitterWidget: widget [
	
	self
		setOffsetsFor: widget
		top: widget topOffset
		left: widget leftOffset
		bottom: widget bottomOffset
		right: widget rightOffset
		borderWidth: self class windowBorderWidth.
	
	widget generateArguments.
	widget asSpecElements do: [ :el | result add: el ]
]

{ #category : #private }
SpecLayout >> computeNotSplitterWidgets [

	(self commands reject: [ :e | e isSplitter ]) 
		do: [ :e | self computeNotSplitterWidget: e ]
]

{ #category : #private }
SpecLayout >> computeSplitters [

	(self commands select: [ :e | e isSplitter ])
		do: [ :e | 
			shouldCheckSplitters := true.
			e generateArguments.
			e asSpecElements do: [ :el | result add: el ] ].
	shouldCheckSplitters
		ifTrue: [ result add: #checkSplitters ]
]

{ #category : #initialization }
SpecLayout >> initialize [ 
	super initialize.
	
	type := #ContainerPresenter.
	self commands: OrderedCollection new.
]

{ #category : #testing }
SpecLayout >> isSpecLayout [

	^ true
]

{ #category : #commands }
SpecLayout >> newColumn: aBlock [ 
	
	self newColumn: aBlock origin: 0@0 corner: 1@1
]

{ #category : #commands-advanced }
SpecLayout >> newColumn: aBlock left: aNumber [
	
	self newColumn: aBlock left: aNumber right: nil
]

{ #category : #commands-advanced }
SpecLayout >> newColumn: aBlock left: left right: right [
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
SpecLayout >> newColumn: aBlock origin: originPoint corner: cornerPoint [ 

	self newColumn: aBlock origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpecLayout >> newColumn: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:c | c add: aBlock ] ].
	
	commands add: (SpecLayoutAddColumn
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asSpecLayoutFrame
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).
]

{ #category : #commands-advanced }
SpecLayout >> newColumn: aBlock right: aNumber [
	
	self newColumn: aBlock left: nil right: aNumber
]

{ #category : #commands }
SpecLayout >> newColumn: aBlock  width: aNumber [
	
	self newColumn: aBlock  origin: 0@0 corner: 0@1 offsetOrigin: 0@0 offsetCorner: aNumber @0
]

{ #category : #commands }
SpecLayout >> newRow: aBlock [ 
	
	self newRow: aBlock origin: 0@0 corner: 1@1
]

{ #category : #commands-advanced }
SpecLayout >> newRow: aBlock bottom: aNumber [
	
	self newRow: aBlock top: nil bottom: aNumber
]

{ #category : #commands }
SpecLayout >> newRow: aBlock height: aNumber [
	
	self newRow: aBlock origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@aNumber 
]

{ #category : #commands }
SpecLayout >> newRow: aBlock origin: originPoint corner: cornerPoint [ 

	self newRow: aBlock origin: originPoint corner: cornerPoint offsetOrigin: 0@0 offsetCorner: 0@0
]

{ #category : #commands }
SpecLayout >> newRow: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint [
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:r | r add: aBlock ] ].
	
	commands add: (SpecLayoutAddRow
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asSpecLayoutFrame 
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).
]

{ #category : #commands-advanced }
SpecLayout >> newRow: aBlock top: aNumber [
	
	self newRow: aBlock top: aNumber bottom: nil
]

{ #category : #commands-advanced }
SpecLayout >> newRow: aBlock top: top bottom: bottom [
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
SpecLayout >> privateAsArray [

	self resetArrayComputation.
	
	self computeNotSplitterWidgets.
	self computeSplitters.
	
	^ result asArray
]

{ #category : #converting }
SpecLayout >> resetArrayComputation [
	result := OrderedCollection new.
	shouldCheckSplitters := false
]

{ #category : #accessing }
SpecLayout >> selector [
	^ selector
]

{ #category : #accessing }
SpecLayout >> selector: anObject [
	selector := anObject
]

{ #category : #commands }
SpecLayout >> send: aSelector [
	
	self send: aSelector withArguments: #()
]

{ #category : #commands }
SpecLayout >> send: aSelector withArguments: arguments [


	self commands add: (SpecLayoutSend
							selector: aSelector
							arguments: arguments).
]

{ #category : #initialization }
SpecLayout >> setBottomOffset: bottom for: widget borderWidth: windowBorderWidth [

	widget bottomFraction = 1
		ifTrue: [ 
			bottom isZero
				ifTrue: [ widget bottomOffset: windowBorderWidth negated ]
				ifFalse: [ widget bottomOffset: bottom - self splitterWidth ] ]
]

{ #category : #initialization }
SpecLayout >> setLeftOffset: left for: widget borderWidth: windowBorderWidth [

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
SpecLayout >> setOffsetsFor: widget top: top left: left bottom: bottom right: right borderWidth: windowBorderWidth [

	self setRightOffset: right for: windowBorderWidth borderWidth: widget.
	self setLeftOffset: left for: widget borderWidth: windowBorderWidth.
	self setBottomOffset: bottom for: widget borderWidth: windowBorderWidth.
	self setTopOffset: top for: widget borderWidth: windowBorderWidth
]

{ #category : #initialization }
SpecLayout >> setRightOffset: right for: windowBorderWidth borderWidth: widget [

	widget rightFraction = 1
		ifTrue: [ 
			right isZero
				ifTrue: [ widget rightOffset: windowBorderWidth negated ]
				ifFalse: [ widget rightOffset: right - self splitterWidth ] ]
]

{ #category : #initialization }
SpecLayout >> setTopOffset: top for: widget borderWidth: windowBorderWidth [

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
SpecLayout >> splitterWidth [
	
	^ 4
]

{ #category : #accessing }
SpecLayout >> type [
	^ type
]

{ #category : #accessing }
SpecLayout >> type: anObject [
	type := anObject
]
