defaults
menuItemDisabledTextColorFor: aMenuItem
	"Answer the color to use for disabled menu item text."

	^((aMenuItem color luminance - aMenuItem owner color luminance) abs < 0.3)
		ifTrue: [aMenuItem owner color contrastingColor muchDarker]
		ifFalse: [aMenuItem owner color muchDarker]