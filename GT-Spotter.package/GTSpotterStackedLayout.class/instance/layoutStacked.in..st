as yet unclassified
layoutStacked: aBrick in: newBounds
	| origStackedBricks stackedBricks forward stackedTopBricks stackedBottomBricks stackedTopHeight stackedBottomHeight|
	self flag: 'refactor me'.
	self allowedBounds: newBounds.
	stackedBricks := self gatherStacked: aBrick.
	origStackedBricks := DoubleLinkedList new.
	origStackedBricks addAll: stackedBricks.
	forward := true.
	
	stackedTopHeight := 0.
	stackedBottomHeight := 0.
	stackedTopBricks := OrderedCollection new.
	stackedBottomBricks := OrderedCollection new.
	
	"going from top to bottom"
	[ stackedBricks isEmpty not & forward] whileTrue: [ |brick|
		brick := stackedBricks removeFirst value.
		(self isVisible: brick) ifFalse: [
			(self isAbove: brick) ifTrue: [ |delta|
				delta := (self deltaTop: brick) - stackedTopHeight.
				brick translateX: delta negated.
				self increaseZindex: brick.
				stackedTopBricks isEmpty ifTrue: [
					stackedTopHeight := stackedTopHeight + brick globalOuterBounds height.
					self cutAllowedBoundsTop: brick ].
				stackedTopBricks add: brick.
			] ifFalse: [ forward := false. stackedBricks addFirst: brick ]
		] ifTrue: [
			(self isPartiallyVisibleAbove: brick)
				ifTrue: [ self layoutAllowedTop: brick ]
				ifFalse: [ forward := false. self decreaseZindex: brick.	stackedBricks addFirst: brick ] ] ].

	"going from bottom to top"
	[ stackedBricks isEmpty not ] whileTrue: [ |brick|
		brick := stackedBricks removeLast value.
		(self isVisible: brick) ifFalse: [ |delta|
			delta := (self deltaBottom: brick) - stackedBottomHeight.
			brick translateX: delta negated.
			self increaseZindex: brick.
			stackedBottomBricks isEmpty ifTrue: [
				stackedBottomHeight := stackedBottomHeight + brick globalOuterBounds height.
				self cutAllowedBoundsBottom: brick ].
			stackedBottomBricks add: brick.
		] ifTrue: [
			(self isFullyVisible: brick )
				ifFalse: [ self layoutAllowedBottom: brick ]
				ifTrue: [ self decreaseZindex: brick ] ] ].

	self layoutProportional: stackedTopBricks reverse in: aBrick position: #top.
	self layoutProportional: stackedBottomBricks reverse in: aBrick position: #bottom.
	
	"origStackedBricks do: [ :each | each doLayout ]."
	
	^ allowedBounds