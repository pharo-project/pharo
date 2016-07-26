border-styles-scrollbars
scrollbarNormalThumbBorderStyleFor: aScrollbar
	"Return the normal thumb borderStyle for the given scrollbar."

	|aColor|
	aColor := self scrollbarColorFor: aScrollbar.
	^BorderStyle simple
		width: 1;
		baseColor: aColor twiceDarker