tests
testPresentationAndBrowserConditions
	| pane presentation1 presentation2 presentation3 |
	pane := GLMPane named: 'test'.
	presentation1 := GLMExplicitBrowser new condition: [ :first :second | first = 1 and: [ second = 2 ] ].
	presentation2 := GLMTabulator new condition: [ :first | first = #foo ].
	presentation3 := GLMExplicitBrowser new useExplicitNotNil.
	pane addPresentation: presentation1.
	pane addPresentation: presentation2.
	pane addPresentation: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first == presentation1.
	self assert: pane matchingPresentations last == presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(#foo 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first == presentation2.
	self assert: pane matchingPresentations last == presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(nil 2)).
	self assert: pane matchingPresentations size equals: 1