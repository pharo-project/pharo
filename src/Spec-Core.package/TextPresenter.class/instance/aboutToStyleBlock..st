api-shout
aboutToStyleBlock: aBlockReturningABoolean
	"<api: #block getter: #aboutToStyleBlock registration: #whenAboutToStyleBlockChanged:>"
	"Set the block used to know if the text must be styled.
	The block must return a boolean"

	aboutToStyleHolder value: aBlockReturningABoolean