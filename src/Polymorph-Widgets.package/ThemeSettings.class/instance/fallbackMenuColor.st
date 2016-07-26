menu
fallbackMenuColor
	"Answer the value of menuColor"

	^ self preferGradientFill ifTrue: [Color lightGray] ifFalse: [Color lightGray muchLighter]