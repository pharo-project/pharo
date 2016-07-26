border-styles-scrollbars
scrollbarNormalFillStyleFor: aScrollbar
	"Return the normal scrollbar fillStyle for the given scrollbar."
	
	^ SolidFillStyle color: self backgroundColor lighter.