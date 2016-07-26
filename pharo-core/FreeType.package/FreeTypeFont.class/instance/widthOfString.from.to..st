measuring
widthOfString: aString from: startIndex to: stopIndex
	"Measure the length of the given string between start and stop index.
	Currently this allows for the right side bearing of the last char, but does not allow for the left side bearing of the first char. We really need a new method - boundingBoxOfString that allows for both. Senders of this will also need to know the LSB of the first char, and position their text accordingly"
	| char nextChar resultX glyph a subPixelPosition |

	a := Array new: 2.
	"FreeTypeSettings current hinting ifFalse:[
		^self linearWidthOfString: aString from: startIndex to: stopIndex]."
	resultX := 0.
	startIndex to: stopIndex do:[:i | 
		char := aString at: i.
		nextChar := (i + 1 <= stopIndex)
			ifTrue:[ aString at: i + 1]
			ifFalse:[nil].
		self widthAndKernedWidthOfLeft: char right:  nextChar into: a. 
		resultX := resultX + (a at:2).
		i = stopIndex 
			ifTrue:[
				subPixelPosition := (((resultX \\ 1) roundTo: "1/64" 0.015625) * 64) asInteger.
				subPixelPosition = 64 
					ifTrue:[
						subPixelPosition := 0.
						resultX := resultX + 1 ].
				subPixelPosition := (subPixelPosition max: 0) min: 63.
				glyph := self glyphOf: char colorValue: 0 mono: FreeTypeSettings current monoHinting subpixelPosition: subPixelPosition.
				glyph ifNotNil:[
					"currently the glyph is too wide. This is to allow for some extra space to ensure 
					the glyph is not clipped when it is produced. Either make the width accurate,
					or hold the RSB value separately, or hold an accurate width separately"
					resultX := resultX "+ 2" + glyph offset x "negated" + (glyph width - (a at: 2)  "glyph linearAdvance x floor")]]].
	^resultX ceiling 