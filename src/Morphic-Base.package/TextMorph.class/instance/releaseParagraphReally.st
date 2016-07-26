private
releaseParagraphReally

	"a slight kludge so subclasses can have a bit more control over whether the paragraph really 
	gets released. important for GeeMail since the selection needs to be accessible even if the 
	hand is outside me"

	"Paragraph instantiation is lazy -- it will be created only when needed"
	self releaseEditor.
	paragraph ifNotNil:
		[paragraph := nil].
	container ifNotNil:
		[container releaseCachedState]