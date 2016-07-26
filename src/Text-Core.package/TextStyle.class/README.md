A textStyle comprises the formatting information for composing and displaying a unit (usually a paragraph) of text.  Typically one makes a copy of a master textStyle (such as TextStyle default), and then that copy may get altered in the process of editing.  Bad things can happen if you do not copy first.

Each of my instances consists of...
	fontArray		An array of StrikeFonts
	fontFamilySize	unused
	lineGrid			An integer; default line spacing for paragraphs
	baseline			An integer; default baseline (dist from line top to bottom of an 'a')
	alignment		An integer; text alignment, see TextStyle alignment:
	firstIndent		An integer; indent of first line in pixels
	restIndent		An integer; indent of remaining lines in pixels
	rightIndent		An integer; indent of right margin rel to section
	tabsArray		An array of integers giving tab offsets in pixels
	marginTabsArray	An array of margin tabs
	leading			An integer giving default vertical line separation

For a concrete example, look at TextStyle default copy inspect