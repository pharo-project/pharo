enumerating
collect: aBlock
	^ self species newFrom:
		(self associations collect: [:each |
			each key -> (aBlock value: each value)])