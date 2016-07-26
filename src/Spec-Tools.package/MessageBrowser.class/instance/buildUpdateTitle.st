initialization
buildUpdateTitle
	"Trying to have a clean subscription, this method is called in #initialize"
	
	self updateTitle.
	textModel text: textModel getText.