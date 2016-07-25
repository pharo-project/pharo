expanding-collapsing
expandAllTo: aDepth
	children do: [ :each | each expandAllTo: aDepth ] displayingProgress: [ :each | 'Expanding ' , each name ] every: 200