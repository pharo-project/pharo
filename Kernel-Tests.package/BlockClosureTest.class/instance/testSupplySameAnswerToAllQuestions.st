tests
testSupplySameAnswerToAllQuestions

	self should: [true = ([self confirm: 'You like Smalltalk?'] valueSupplyingAnswer: true)].
	
	self should: [#(true true) = ([{self confirm: 'One'. self confirm: 'Two'}] valueSupplyingAnswer: true)].