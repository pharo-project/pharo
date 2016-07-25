adding
addNullLineForIndex: index
	"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	| oldLastLine r |
	oldLastLine := lines last.
	oldLastLine last - oldLastLine first >= 0
		ifFalse: [ ^ self ].
	oldLastLine last = (index - 1)
		ifFalse: [ ^ self ].
	r := oldLastLine left @ oldLastLine bottom extent: oldLastLine right @ (oldLastLine bottom - oldLastLine top).	
	"Even though we may be below the bottom of the container,
	it is still necessary to compose the last line for consistency..."
	self addNullLineWithIndex: index andRectangle: r