tests
testUntypedSelectorsOnly
	| text context model |
	text := 'testIt: aRectangle
	| ab bc bd |
	ab '.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'.
	context narrowWith: 'a'.
	self assert: (model entriesOfType: #selector) notEmpty.
	self assert: (model entriesOfType: #local) isEmpty.
	self assert: (model entriesOfType: #instance) isEmpty