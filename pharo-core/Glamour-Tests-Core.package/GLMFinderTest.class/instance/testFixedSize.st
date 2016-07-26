tests
testFixedSize
	| browser | 
	browser := GLMFinder new. 
	self assert: browser hasFixedSizePanes.
	browser hasFixedSizePanes: false.
	self assert: browser hasFixedSizePanes not	