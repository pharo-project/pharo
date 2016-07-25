fill-styles-scrollbars
scrollbarNormalThumbFillStyleFor: aScrollbar
	"Return the normal scrollbar thumb fillStyle for the given scrollbar."
	
	^aScrollbar bounds isWide
		ifTrue: [self scrollbarNormalHorizontalThumbFillStyleFor: aScrollbar]
		ifFalse: [self scrollbarNormalVerticalThumbFillStyleFor: aScrollbar]