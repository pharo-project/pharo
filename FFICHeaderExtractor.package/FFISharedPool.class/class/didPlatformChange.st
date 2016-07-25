testing
didPlatformChange
	"Answers whether this shared pool was initialized for the same platform as the current one. In other words, it answers whether the platform has changed."
	^ ((self platformName = self currentPlatformName) and: [ self wordSize = self currentWordSize ]) not 