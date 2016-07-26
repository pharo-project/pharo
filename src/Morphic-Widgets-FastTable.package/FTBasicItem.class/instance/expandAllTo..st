expanding-collapsing
expandAllTo: aDepth
	self expand.
	aDepth > self depth
		ifTrue: [ self children do: [ :each | each expandAllTo: aDepth ] ]