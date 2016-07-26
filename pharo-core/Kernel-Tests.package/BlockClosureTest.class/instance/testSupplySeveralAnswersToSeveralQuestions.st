tests
testSupplySeveralAnswersToSeveralQuestions

	self should: [#(false true) = ([{self confirm: 'One'. self confirm: 'Two'}] 
		valueSupplyingAnswers: #( ('One' false) ('Two' true) ))].
	
	self should: [#(true false) = ([{self confirm: 'One'. self confirm: 'Two'}] 
		valueSupplyingAnswers: #( ('One' true) ('Two' false) ))]