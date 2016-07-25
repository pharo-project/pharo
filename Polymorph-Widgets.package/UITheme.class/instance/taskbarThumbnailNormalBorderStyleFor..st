border-styles
taskbarThumbnailNormalBorderStyleFor: aWindow
	"Return the normal thumbnail borderStyle for the given button."

	|c|
	c := self windowColorFor: aWindow.
	^BorderStyle raised
		width: 1;
		baseColor: c