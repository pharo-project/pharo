accessing
addDecorator: morph
	decorators add: morph.
	self addMorph: morph.
	morph setContainer: self.