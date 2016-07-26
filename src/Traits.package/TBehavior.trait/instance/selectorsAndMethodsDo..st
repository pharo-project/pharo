accessing method dictionary
selectorsAndMethodsDo: selectorAndMethodBlock
	"Evaluate selectorAndMethodBlock with two arguments for each selector/method pair in my method dictionary."

	^ self methodDict keysAndValuesDo: selectorAndMethodBlock