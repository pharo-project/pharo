private
abstractColorAdjust: aColor
			
	^aColor luminance > 0.5
			ifTrue: [ aColor twiceDarker ]
			ifFalse: [ aColor twiceLighter ]