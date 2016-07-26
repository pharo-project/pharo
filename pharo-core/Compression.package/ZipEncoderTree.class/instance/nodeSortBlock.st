encoding
nodeSortBlock
	^[:n1 :n2|
		n1 frequency = n2 frequency
			ifTrue:[n1 height <= n2 height]
			ifFalse:[n1 frequency <= n2 frequency]].