tests
testEmptyPanes
	| browser | 
	browser := GLMFinder new. 
	self assert: browser panes isEmpty. 
	self assert: browser transmissions isEmpty.