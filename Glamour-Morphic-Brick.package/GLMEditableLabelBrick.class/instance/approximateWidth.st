geometry
approximateWidth
	
	^ self hMargin + self hPadding + (self isReadMode
		ifTrue: [ self labelBrick approximateWidth ]
		ifFalse: [ self editBrick width + self editBrick hPadding + self editBrick hMargin])