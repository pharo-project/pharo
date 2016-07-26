as yet unclassified
initializeWithFontArray: anArray 
	"Initialize with given font array, the ascent of primary font is modified 
	if another font has higher size"
	| primaryFont maxHeight newFont |
	fontArray := anArray.
	primaryFont := anArray at: 1.
	emphasis := 0.
	name := primaryFont name.
	maxHeight := anArray
				inject: 0
				into: [:theHeight :font | (font notNil
							and: [theHeight < font height])
						ifTrue: [font height]
						ifFalse: [theHeight]].
	primaryFont height < maxHeight
		ifTrue: [newFont := primaryFont copy
						fixAscent: primaryFont ascent + (maxHeight - primaryFont height)
						andDescent: primaryFont descent
						head: 0.
			fontArray at: 1 put: newFont].
	self reset