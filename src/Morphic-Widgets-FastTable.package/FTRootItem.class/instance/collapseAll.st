expanding-collapsing
collapseAll
	children do: #collapseAll displayingProgress: [ :each | 'Collapsing ' , each name ] every: 200