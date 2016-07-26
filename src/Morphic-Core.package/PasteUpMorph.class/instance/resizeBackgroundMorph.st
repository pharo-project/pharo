recategorized
resizeBackgroundMorph
	"Resize the background morph to fit the world."

	self backgroundMorph isNil ifFalse: [
		self backgroundMorph extent: self extent]