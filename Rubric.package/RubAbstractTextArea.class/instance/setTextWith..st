public accessing
setTextWith: stringOrText
	"Accept new text contents."

	self handleParagraphChange: [self privateText: stringOrText]