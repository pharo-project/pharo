layout
minHeight
	"Answer the receiver's minHeight"
	
	^owner isVertical
				ifTrue: [ self theme menuBorderWidth ]
				ifFalse: [10]
	