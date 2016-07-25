A CharacterScanner holds the state associated with scanning text. Subclasses scan characters for specified purposes, such as computing a CharacterBlock or placing characters into Forms.

Instance Variables
	alignment:		<Integer>
	destX:		<Number>
	destY:		<Number>
	emphasisCode:		<Object>
	font:		<AbstractFont>
	indentationLevel:		<Integer>
	kern:		<Number>
	lastIndex:		<Integer>
	leftMargin:		<Number>
	line:		<TextLine>
	map:		<Array>
	pendingKernX:		<Number>
	rightMargin:		<Number>
	runStopIndex:		<Integer>
	spaceCount:		<Integer>
	spaceWidth:		<Number>
	stopConditions:		<Array>
	text:		<Text>
	textStyle:		<TextStyle>
	wantsColumnBreaks:		<Boolean>
	xTable:		<Array>

alignment
	- an Integer encoding the alignment of text

destX
	- horizontal position for next character (distance from left of composition area)

destY
	- vertical position for next character (distance from top of composition area)

emphasisCode
	- an Integer encoding the current text emphasis to use (bold, italic, ...)

font
	- the current font used for measuring/composing/displaying characters

indentationLevel
	- an Integer specifying a number of leading tabs to be inserted at beginning of new lines

kern
	- a Number specifying additional horizontal spacing to place between characters (spacing is reduced when kern is negative)

lastIndex
	- the Integer index of next character to be processed in the text

leftMargin
	- a Number specifying the distance between left of composition zone and left of first character in the line.

line
	- an object holding information about the line currently being displayed (like first and last index in text).
	Note: this is either a TextLine in Morphic, or TextLineInterval for ST80 compatibility

map
	- an array mapping character code to glyph position.
	This is used by primitive 103 only, in case of ByteString.

pendingKernX
	- a Number to be added to horizontal spacing of next char if ever it is in the same font than previous one.
	The inner scan loop is interrupted by a change of text run.
	But some changes won't change the font, so the kerning must be remembered and applied later.

rightMargin
	- a Number specifying the distance between right of composition zone and right of last character in the line.

runStopIndex
	- the Integer index of last character in current text run.

spaceCount
	- the number of spaces encoutered so far in current line. This is useful for adjusting the spacing in cas of Justified alignment.

spaceWidth
	- the width of space character in current font.

stopConditions
	- an Array mapping a table of characters codes for which special actions are to be taken.
	These are typically control characters like carriage return or horizontal tab.

text
	- the text to be measured/composed/displayed

textStyle
	- an object holding a context for the text style (which set of font to use, which margins, etc...)

wantsColumnBreaks
	- a Boolean indicating whether some special handling for multiple columns is requested.
	THIS ONLY MAKES SENSE IN CompositionScanner AND SHOULD BE MOVED TO THE SUBCLASS
	
xTable
	- an array mapping character code to glyph x coordinate in form.
	This is used by primitive 103 only, in case of ByteString.
	
Implementation note: accelerated Character scanning with primitive 103 requires following order for 5 first instance variables, please don't alter:
destX lastIndex xTable map destY
