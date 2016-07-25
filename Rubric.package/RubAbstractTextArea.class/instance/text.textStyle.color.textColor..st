private
text: t textStyle: s color: c textColor: tc
	"Private -- for use only in morphic duplication"	
	self releaseParagraph.
	super text: t textStyle: s color: c textColor: tc.
