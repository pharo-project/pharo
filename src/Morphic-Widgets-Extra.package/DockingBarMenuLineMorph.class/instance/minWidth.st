layout
minWidth
	"Answer the receiver's minWidth"

	^ owner isVertical
		ifTrue: [ 10 ]
		ifFalse: [ self theme menuBorderWidth ]