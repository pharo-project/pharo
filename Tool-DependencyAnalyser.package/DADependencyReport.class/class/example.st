example
example
	^ self new 
		add: #A dependencies: #();
		add: #B dependencies: #(#A);
		add: #C dependencies: #(#A #B);
		add: #D dependencies: #(#C #A #E);
		add: #E dependencies: #(#D);
		yourself