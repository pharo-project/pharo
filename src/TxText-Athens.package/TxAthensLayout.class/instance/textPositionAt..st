geometry
textPositionAt: aPoint
	| line height index |
	self numberOfLines = 0
		ifTrue: [ ^ 0 ].	"find the line under the aPoint y by: accumulating the heights starting from the first line of the layout,  till we reach the Y of the point"
	aPoint y <= 0
		ifTrue: [ ^ lines first textPositionAtHorizontalOffset: aPoint x ].
	height := 0.
	index := 1.
	[ index <= lines size and: [ height < aPoint y ] ]
		whileTrue: [ 
			line := lines at: index.
			height := height + line height.
			index := index + 1 ].
	index > lines size
		ifTrue: [ ^ lines last textPositionAtHorizontalOffset: aPoint x ].
	^ line textPositionAtHorizontalOffset: aPoint x