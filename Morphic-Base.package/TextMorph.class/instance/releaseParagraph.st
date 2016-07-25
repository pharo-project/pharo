private
releaseParagraph

	"a slight kludge so subclasses can have a bit more control over whether the paragraph really 
	gets released. important for GeeMail since the selection needs to be accessible even if the 
	hand is outside me"

	self releaseParagraphReally.
