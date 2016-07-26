tests
testSupplyAnswerThroughNestedBlocks

	self should: [true = ([[self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('Blub' false)] valueSupplyingAnswer: #('Smalltalk' true))]