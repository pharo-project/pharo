tests
testCompositeInTheParentPane
	| browser |
	browser := GLMBrowser new.
	self assert: (browser pane presentations isKindOf: GLMCompositePresentation).
	self assert: browser pane presentations size equals: 1.
	self assert: browser pane presentations first equals: browser