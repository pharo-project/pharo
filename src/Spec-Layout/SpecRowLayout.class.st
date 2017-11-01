"
A SpecRowLayout isthe layout used to specify a new row
"
Class {
	#name : #SpecRowLayout,
	#superclass : #SpecLayout,
	#instVars : [
		'width'
	],
	#category : 'Spec-Layout'
}

{ #category : #commands }
SpecRowLayout >> add: aSpec width: aNumber [
	
	self add: aSpec origin: 0@0 corner: 1@1 offsetOrigin: 0@0 offsetCorner: aNumber @0
]

{ #category : #commands }
SpecRowLayout >> addSplitter [

	self commands add: (SpecSplitterVertical commands: self commands)
]

{ #category : #private }
SpecRowLayout >> autoFractionComputation [
	
	self commands
		doWithIndex: [ :e :idx | 
			e layoutFrame hasNoOffsets
				ifTrue: [ | delta |
					delta := e layoutFrame rightFraction - e layoutFrame leftFraction.
					self setWidgetLayoutWithNoOffset: e at: idx delta: delta ]
				ifFalse: [ self setWidgetLayoutWithOffset: e at: idx ] ]
]

{ #category : #private }
SpecRowLayout >> countNumberOfProportionals [

	self commands do: [ :e | 
		e layoutFrame hasNoOffsets
			ifTrue: [ 
				(e layoutFrame leftFraction == 0 and: [ e layoutFrame rightFraction == 1 ])
					ifFalse: [ autoFractionComputation := false ].
				numberOfProportionals := numberOfProportionals + 1 ] ]
]

{ #category : #private }
SpecRowLayout >> manualFractionComputation [
	| delta |
	delta := numberOfProportionals isZero
		ifTrue: [ 0 ]
		ifFalse: [ 1 / numberOfProportionals ].
	self commands
		doWithIndex: [ :e :idx | 
			e layoutFrame hasNoOffsets
				ifTrue: [ self setWidgetLayoutWithNoOffset: e at: idx delta: delta ]
				ifFalse: [ self setWidgetLayoutWithOffset: e at: idx ] ]
]

{ #category : #converting }
SpecRowLayout >> privateAsArray [

	self resetArrayComputation.
	self countNumberOfProportionals.
	
	autoFractionComputation
		ifTrue: [ self manualFractionComputation ]
		ifFalse: [ self autoFractionComputation ].
	
	self shiftLastWidgets.
	
	result addAll: super privateAsArray.
	^ result asArray
]

{ #category : #private }
SpecRowLayout >> resetArrayComputation [

	super resetArrayComputation.
	
	currentOffset := 0.
	currentProportional := 0.
	numberOfProportionals := 0.
	lastFractionIndex := 0.
	autoFractionComputation := true.
	size := self commands size
]

{ #category : #private }
SpecRowLayout >> setWidgetLayoutWithNoOffset: widget at: index delta: delta [

	widget layoutFrame: ((currentProportional @ 0 corner: (currentProportional + delta) @ 1) asSpecLayoutFrame leftOffset: currentOffset).
	
	currentOffset := 0.
	currentProportional := currentProportional + delta.
	lastFractionIndex := index
]

{ #category : #private }
SpecRowLayout >> setWidgetLayoutWithOffset: e at: idx [
	| y0 y1 |
	
	width := e offsets width.
	y0 := e offsets top.
	y1 := e offsets bottom.
	
	idx = size
		ifTrue: [ 
			e layoutFrame:
				((1 @ 0 corner: 1 @ 1) asSpecLayoutFrame
					topLeftOffset: width negated @ y0;
					bottomRightOffset: 0 @ y1) ]
		ifFalse: [ 
			e layoutFrame:
				((currentProportional @ 0 corner: currentProportional @ 1) asSpecLayoutFrame
					topLeftOffset: currentOffset @ y0;
					bottomRightOffset: (currentOffset + width) @ y1).
			currentOffset := currentOffset + width ]
]

{ #category : #private }
SpecRowLayout >> shiftLastWidgets [
	| index ante |
	
	(lastFractionIndex > 0 and: [ lastFractionIndex < size ]) ifFalse: [ ^ self ].
	
	index := size - 1.
	[ index > lastFractionIndex ] whileTrue: [ | myWidth |
		ante := self commands at: index.
		index := index - 1.
		myWidth := ante rightOffset - ante leftOffset.
		ante
			leftOffset: (width + myWidth) negated;
			rightOffset: width negated.
		width := width + myWidth ].
	
	self assert: index = lastFractionIndex.
	
	ante := self commands at: index.
	ante layoutFrame rightOffset: width negated
]
