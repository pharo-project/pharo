expanding-collapsing
expandAll
	children do: #expandAll displayingProgress: [ :each | 'Expanding ' , each printString ] every: 200