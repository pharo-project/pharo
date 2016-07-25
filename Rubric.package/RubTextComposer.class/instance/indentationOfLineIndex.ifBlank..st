querying
indentationOfLineIndex: lineIndex ifBlank: aBlock
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last crlf |
	crlf := CharacterSet crlf.
	arrayIndex := lineIndex.
	[first := (self lines at: arrayIndex) first.
	 first > 1 and: [crlf includes: (self text string at: first - 1)]] whileTrue: "word wrap"
		[arrayIndex := arrayIndex - 1].
	last := (self lines at: arrayIndex) last.
	
	^(self text string copyFrom: first to: last) indentationIfBlank: aBlock.
