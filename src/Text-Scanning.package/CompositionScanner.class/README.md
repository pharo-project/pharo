A CompositionScanner measures text and determines where line breaks.
Given a rectangular zone on input, it is used to split text in horizontal lines, and produce information about those lines on output (at which index a line starts/stops, which vertical space does the line require, which horizontal space if left for adjusting inter-word spacing, etc...)

Instance Variables
	baseline:		<Number>
	baselineAtSpace:		<Number>
	lastBreakIsNotASpace:		<Boolean>
	lineHeight:		<Number>
	lineHeightAtSpace:		<Number>
	nextIndexAfterLineBreak:		<Integer>
	spaceIndex:		<Integer>
	spaceX:		<Number>

baseline
	- the distance between top of line and the base line (that is the bottom of latin characters abcdehiklmnorstuvwx in most fonts)

baselineAtSpace
	- memorize the baseline at last encountered space or other breakable character.
	This is necessary because the CompositionScanner wants to break line at a breakable character.
	If a word layout overflows the right margin, the scanner has to roll back and restore the line state to last encountered breakable character.

lastBreakIsNotASpace
	- indicates that the last breakable character was not a space.
	This is necessary because handling a line break at a space differs from non space.
	If line break occurs on space, the space won't be displayed in next line.
	If it's another breakable character, it has to be displayed on next line.

lineHeight
	- the total line height from top to bottom, including inter-line spacing.

lineHeightAtSpace
	- the line height at last encountered space or other breakable character.
	See baselineAtSpace for explanation.

nextIndexAfterLineBreak
	- the index of character after the last line break that was encountered.

spaceIndex
	- the index of last space or other breakable character that was encountered

spaceX
	- the distance from left of composition zone to left of last encountered space or other breakable character 
	See baselineAtSpace for explanation.

Note: if a line breaks on a space, a linefeed or a carriage return, then the space, linefeed or carriage return is integrated in the line.
If there is a carriage return - linefeed pair, the pair is integrated to the line as if it were a single line break for compatibility with legacy software.