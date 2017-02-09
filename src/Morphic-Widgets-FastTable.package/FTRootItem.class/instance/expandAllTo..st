expanding-collapsing
expandAllTo: aDepth
	children do: [ :each | each expandAllTo: aDepth ]