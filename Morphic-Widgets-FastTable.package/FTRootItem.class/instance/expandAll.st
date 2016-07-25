expanding-collapsing
expandAll
	children do: #expandAll displayingProgress: [ :each | 'Expanding ' , each name ] every: 200