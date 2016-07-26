defaults
menuItemNormalTextColorFor: aMenuItem
	"Answer the color to use for normal menu item text."

	^((aMenuItem color luminance - aMenuItem owner paneColor luminance) abs < 0.3)
		ifTrue: [aMenuItem owner paneColor contrastingColor]
		ifFalse: [aMenuItem color]