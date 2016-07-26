accessing
bounds
	"Answer the bounds of the receiver."

	^bounds ifNil: [bounds := self calculatedBounds]