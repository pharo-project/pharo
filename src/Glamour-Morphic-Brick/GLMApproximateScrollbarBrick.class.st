"
A GLMApproximateScrollbarBrick can be used only with GLMScrollListBrick or with other lists that uses adaptors.

Approximates the height of band depending on average height of all visible items.

"
Class {
	#name : #GLMApproximateScrollbarBrick,
	#superclass : #GLMScrollbarBrick,
	#category : #'Glamour-Morphic-Brick-Widgets-Scrollpane'
}

{ #category : #'scrollbar-calculations' }
GLMApproximateScrollbarBrick >> approximateBandHeight [
	|visible averageItemHeight|
	
	visible := self scrollPane band submorphs select: [ :each | self scrollPane isPartiallyVisible: each ].
	visible isEmpty ifTrue: [ ^ 0 ].
	
	averageItemHeight := (visible inject: 0 into: [ :visibleHeight :each | visibleHeight + each height ]) / visible size.
	^ self scrollPane model count * averageItemHeight.
]

{ #category : #'scrollbar-calculations' }
GLMApproximateScrollbarBrick >> calculateScrollbarHeight [
	|bandHeight firstIndex lastIndex |
	bandHeight := self approximateBandHeight.
	bandHeight = 0 ifTrue: [ ^ 0 ].
	self scrollPane isFullyVisible ifTrue: [ ^ 0 ].
	
	firstIndex := self scrollPane getIndex: (self scrollPane band firstSubbrick).
	lastIndex := self scrollPane getIndex: (self scrollPane band lastSubbrick).
	
	^ ((lastIndex - firstIndex + 1) / self scrollPane model count * self height) ceiling.
	
]

{ #category : #'scrollbar-calculations' }
GLMApproximateScrollbarBrick >> calculateScrollbarTopOffset [
	|bandHeight firstIndex lastIndex|
	bandHeight := self approximateBandHeight.
	bandHeight = 0 ifTrue: [ ^ 0 ].
	
	firstIndex := self scrollPane getIndex: (self scrollPane band firstSubbrick).
	lastIndex := self scrollPane getIndex: (self scrollPane band lastSubbrick).
	
	^ ((firstIndex-1) / self scrollPane model count * self height) floor.
]
