example
exampleSequenceMethods
	<gtExample>
	<label: 'You should not do this. Instead subclass and call #add:… in the initialization method'>
	
	^ self new
		add: #initiallize requiresPreSend: #(basicNew new);
		add: #(setAsFixed setAsNotFixed) requiresPostSend: #(add: addAll:);
		add: #edges requiresPreSend: #nodes