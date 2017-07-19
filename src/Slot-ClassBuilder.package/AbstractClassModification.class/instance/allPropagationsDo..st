accessing
allPropagationsDo: aBlock
	propagations do: [ :aPropagation |
		aBlock value: aPropagation.
		aPropagation allPropagationsDo: aBlock ]