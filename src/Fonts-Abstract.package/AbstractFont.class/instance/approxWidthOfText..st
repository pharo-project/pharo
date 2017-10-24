measuring
approxWidthOfText: aText 
	"Return the width of aText -- quickly, and a little bit dirty. Used by lists morphs containing Text objects to get a quick, fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText size = 0 ]) ifTrue: [ ^ 0 ].
	w := self widthOfString: aText asString.

	"If the text has no emphasis, just return the string size.  If it is empasized, 
    just approximate the width by adding about 20% to the width"
	((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ]) 
		ifTrue: [ ^ w ]
		ifFalse: [ ^ w * 6 // 5 ]