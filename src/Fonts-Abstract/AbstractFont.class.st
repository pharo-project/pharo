"
The abstract class AbstractFont defines the generic interface that all fonts need to implement.
"
Class {
	#name : 'AbstractFont',
	#superclass : 'Object',
	#category : 'Fonts-Abstract-Base',
	#package : 'Fonts-Abstract',
	#tag : 'Base'
}

{ #category : 'testing' }
AbstractFont class >> isAbstract [

	^self == AbstractFont
]

{ #category : 'measuring' }
AbstractFont >> approxWidthOfText: aText [
	"Return the width of aText -- quickly, and a little bit dirty. Used by lists morphs containing Text objects to get a quick, fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText size = 0 ]) ifTrue: [ ^ 0 ].
	w := self widthOfString: aText asString.

	"If the text has no emphasis, just return the string size.  If it is empasized,
    just approximate the width by adding about 20% to the width"
	^ ((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ])
		ifTrue: [ w ]
		ifFalse: [ w * 6 // 5 ]
]

{ #category : 'accessing' }
AbstractFont >> ascent [

	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFont >> ascentOf: aCharacter [

	^ self ascent
]

{ #category : 'accessing' }
AbstractFont >> baseKern [
	^0
]

{ #category : 'accessing' }
AbstractFont >> basicAscentOf: aCharacter [

	^ self ascent
]

{ #category : 'accessing' }
AbstractFont >> basicDescentOf: aCharacter [

	^ self descent
]

{ #category : 'glyph lookup' }
AbstractFont >> characterFormAt: aCharacter [
	^ nil
]

{ #category : 'glyph lookup' }
AbstractFont >> characterRenderingOptimizedFormAt: aCharacter [
	"This method returns a version of the character form which is optimized for rendering
	through an accelerated alpha blending equation in the style of the OpenGL glBlendFuncSeparate."

	^ (self characterFormAt: aCharacter)
			ifNotNil: [:form | form invertedAndAlphaMultiplied]
]

{ #category : 'accessing' }
AbstractFont >> characterToGlyphMap [
	"Return the character to glyph mapping table. If the table is not provided the character scanner will query the font directly for the width of each individual character."

	^nil
]

{ #category : 'accessing' }
AbstractFont >> derivativeFonts [
	^#()
]

{ #category : 'accessing' }
AbstractFont >> descent [

	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFont >> descentOf: aCharacter [

	^ self descent
]

{ #category : 'displaying' }
AbstractFont >> displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [

	^ self displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint scale: 1
]

{ #category : 'displaying' }
AbstractFont >> displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint scale: scale [
	"display the strikeout if appropriate for the receiver"
]

{ #category : 'displaying' }
AbstractFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta [
	"Draw the given string from startIndex to stopIndex
	at aPoint on the (already prepared) display context."

	^ self displayString: aString on: aDisplayContext from: startIndex to: stopIndex
		at: aPoint kern: kernDelta baselineY: aPoint y + self ascent
]

{ #category : 'displaying' }
AbstractFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [

	^ self displayString: aString on: aDisplayContext from: startIndex to: stopIndex
		at: aPoint kern: kernDelta baselineY: baselineY scale: 1
]

{ #category : 'displaying' }
AbstractFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY scale: scale [
	"Draw the given string from startIndex to stopIndex at aPoint on the (already prepared) display context."

	^self subclassResponsibility
]

{ #category : 'displaying' }
AbstractFont >> displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [

	^ self displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint scale: 1
]

{ #category : 'displaying' }
AbstractFont >> displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint scale: scale [
	"display the underline if appropriate for the receiver"
]

{ #category : 'accessing' }
AbstractFont >> familyName [
	"Answer the name to be used as a key in the TextConstants dictionary."

	^self subclassResponsibility
]

{ #category : 'testing' }
AbstractFont >> hasDistinctGlyphsForAll: asciiString [
	"Answer true if the receiver has glyphs for all the characters
	in asciiString and no single glyph is shared by more than one character, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"

	^true
]

{ #category : 'testing' }
AbstractFont >> hasSubPixelAntiAliasing [

	^ false
]

{ #category : 'accessing' }
AbstractFont >> height [
	"Answer the height of the receiver, total of maximum extents of
	characters above and below the baseline."

	^self subclassResponsibility
]

{ #category : 'displaying' }
AbstractFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor [

	^ self installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor scale: 1
]

{ #category : 'displaying' }
AbstractFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [
	"Install the receiver on the given DisplayContext (either BitBlt or Canvas) for further drawing operations."

	^ self subclassResponsibility
]

{ #category : 'accessing' }
AbstractFont >> isRegular [

	^ false
]

{ #category : 'testing' }
AbstractFont >> isSubPixelPositioned [
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text.
	Currently, only FreeTypeFonts are subPixelPositioned, and only when not
	Hinted"

	^false
]

{ #category : 'testing' }
AbstractFont >> isSymbolFont [
	"Answer true if the receiver is a Symbol font, false otherwise.
	The default is to answer false, subclasses can reimplement"

	^false
]

{ #category : 'testing' }
AbstractFont >> isTTCFont [

	^ false
]

{ #category : 'measuring' }
AbstractFont >> kerningLeft: leftChar right: rightChar [
	^0
]

{ #category : 'accessing' }
AbstractFont >> lineGrid [
	"Answer the relative space between lines"

	^self subclassResponsibility
]

{ #category : 'measuring' }
AbstractFont >> linearWidthOf: aCharacter [
	"This is the scaled, unrounded advance width."
	^self widthOf: aCharacter
]

{ #category : 'notifications' }
AbstractFont >> pixelsPerInchChanged [
	"The definition of TextStyle class>>pixelsPerInch has changed. Do whatever is necessary."
]

{ #category : 'accessing' }
AbstractFont >> pointSize [
	self subclassResponsibility
]

{ #category : 'caching' }
AbstractFont >> releaseCachedState [
]

{ #category : 'metrics' }
AbstractFont >> strikeoutThickness [
	^ 0
]

{ #category : 'metrics' }
AbstractFont >> strikeoutTop [
	^ 0
]

{ #category : 'accessing' }
AbstractFont >> textStyleName [
	"Answer the name to be used as a key in the TextConstants dictionary."
	^self familyName
]

{ #category : 'metrics' }
AbstractFont >> underlineThickness [
	^ 0
]

{ #category : 'metrics' }
AbstractFont >> underlineTop [
	^ 0
]

{ #category : 'development support' }
AbstractFont >> validate [
	"Concrete classes should override this to provide validation"
]

{ #category : 'measuring' }
AbstractFont >> widthAndKernedWidthOfLeft: leftCharacter right: rightCharacterOrNil into: aTwoElementArray [
	"Set the first element of aTwoElementArray to the width of leftCharacter and
	the second element to the width of left character when kerned with
	rightCharacterOrNil. Answer aTwoElementArray"
	| w k |
	w := self widthOf: leftCharacter.
	rightCharacterOrNil
		ifNil: [
			aTwoElementArray
				at: 1 put: w;
				at: 2 put: w]
		ifNotNil: [
			k := self kerningLeft: leftCharacter right: rightCharacterOrNil.
			aTwoElementArray
				at: 1 put: w;
				at: 2 put: w+k].
	^ aTwoElementArray
]

{ #category : 'measuring' }
AbstractFont >> widthOf: aCharacter [
	"Return the width of the given character"
	^self subclassResponsibility
]

{ #category : 'measuring' }
AbstractFont >> widthOfString: aString [
	"
		TextStyle default defaultFont widthOfString: 'zort' 21
	"
	aString ifNil: [^0].
	^self widthOfString: aString from: 1 to: aString size
]

{ #category : 'measuring' }
AbstractFont >> widthOfString: aString from: firstIndex to: lastIndex [
	"Measure the length of the given string between start and stop index"
	| resultX |
	resultX := 0.
	firstIndex
		to: lastIndex
		do: [ :i | resultX := resultX + (self widthOf: (aString at: i)) ].
	^ resultX
]

{ #category : 'measuring' }
AbstractFont >> widthOfStringOrText: aStringOrText [

    aStringOrText ifNil: [ ^0 ].
    ^aStringOrText isText
        ifTrue:[ self approxWidthOfText: aStringOrText ]
        ifFalse:[ self widthOfString: aStringOrText ]
]

{ #category : 'accessing' }
AbstractFont >> xTable [
	"Return the xTable for the font. The xTable defines the left x-value for each individual glyph in the receiver. If such a table is not provided, the character scanner will ask the font directly for the appropriate width of each individual character."

	^nil
]
