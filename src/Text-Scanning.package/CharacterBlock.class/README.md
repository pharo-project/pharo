I describe the location of one character displayed on the screen. My instances are used to return the results of methods:
	Paragraph characterBlockAtPoint: aPoint and
	Paragraph characterBlockForIndex: stringIndex.
Any recomposition or movement of a Paragraph can make the information I store stale.

text (Text): The text where my character is from
stringIndex (Integer): The index of my character in the text, starting from 1
textLine (TextLine): The displayed line my character is on
origin (Point): The top-left corner of the area allocated for displaying my
		character's glyph, in pixels, counting right then down from the
		top-left corner of the text display area, and starting from 0@0
corner (Point): The bottom-right corner of the area allocated for displaying my
		character's glyph, in pixels, counting right then down from the
		top-left corner of the text display area, and starting from 0@0
