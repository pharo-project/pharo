A DisplayScanner is an abstract class for displaying characters.
It is splitting text into elementary chunks of displayable String/Font pairs (see scanning protocol).
Subclasses responsibility is to handle the effective rendering of these chunks on various backends.

Instance Variables
	backgroundColor:		<Color>
	defaultTextColor:		<Color>
	foregroundColor:		<Color>
	ignoreColorChanges:		<Boolean>
	lastDisplayableIndex:		<Integer>
	lineY:		<Number>
	morphicOffset:		<Point>
	stopConditionsMustBeReset:		<Boolean>

backgroundColor
	- the background color for displaying next chunk of text.
	Note that this can be set to Color transparent, in which case no background is displayed.

defaultTextColor
	- the default foreground color for displaying text in absence of other text attributes specification 

foregroundColor
	- the foreground color for displaying next chunk of text

ignoreColorChanges
	- indicates that any change of color specified in text attributes shall be ignored.
	This is used for displaying text in a shadow mode, when dragging text for example.

lastDisplayableIndex
	- the index of last character to be displayed.
	A different index than lastIndex is required in order to avoid display of control characters.
	This variable must be updated by the stop condition at each inner scan loop.

lineY
	- the distance between destination form top and current line top

morphicOffset
	- an offset for positionning the embedded morphs.
	THE EXACT SPECIFICATION YET REMAINS TO BE WRITTEN

stopConditionsMustBeReset
	- indicates that it's necessary to call setStopConditions in next scan loop.

Notes:
In order to correctly set the lastDisplayableIndex, the display scanner performs the stopCondition BEFORE displaying the string being scanned.
This explains why the stopCondition must not reset the font immediately, but differ this reset AFTER the display, thanks to stopConditionsMustBeReset.
