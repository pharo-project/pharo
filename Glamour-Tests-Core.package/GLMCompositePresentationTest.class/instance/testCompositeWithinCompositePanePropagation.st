as yet unclassified
testCompositeWithinCompositePanePropagation
	| composite0 composite1 composite21 composite22 pane |
	composite0 := GLMCompositePresentation new.
	composite1 := GLMCompositePresentation new.
	composite21 := GLMCompositePresentation new.
	composite22 := GLMCompositePresentation new.
	composite21 add: GLMPresentation new.
	composite1 add: composite21.
	composite22 add: GLMPresentation new.
	composite1 add: composite22.
	composite0 add: composite1.
	pane := GLMPane new.
	pane presentations: composite0.
	self assert: composite0 first first first pane equals: pane.
	self assert: (composite0 first at: 2) pane equals: pane.
	self assert: composite0 first first first pane equals: pane.
	self assert: (composite0 first at: 2) first pane equals: pane