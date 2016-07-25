fill-styles-scrollbars
scrollbarNormalThumbFillStyleFor: aScrollbar
	"Return the normal scrollbar fillStyle for the given scrollbar."
	^ self 
		glamorousVisibleFillStyleFor: aScrollbar 
		height: aScrollbar height