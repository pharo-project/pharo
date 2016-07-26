initialization
initialize
	super initialize.
	textModel := self defaultTextModel.
	encoding := TextConverter default class encodingNames first.
	self initializeBindings