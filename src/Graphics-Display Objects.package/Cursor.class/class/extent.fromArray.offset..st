instance creation
extent: extentPoint fromArray: anArray offset: offsetPoint 
	"Answer a new instance of me with width and height specified by
	extentPoint, offset by offsetPoint, and bits from anArray.
	NOTE: This has been kluged to take an array of 16-bit constants,
	and shift them over so they are left-justified in a 32-bit bitmap"

	extentPoint = (16 @ 16)
		ifTrue: 
			[^ super
				extent: extentPoint
				fromArray: (anArray collect: [:bits | bits bitShift: 16])
				offset: offsetPoint]
		ifFalse: [self error: 'cursors must be 16@16']