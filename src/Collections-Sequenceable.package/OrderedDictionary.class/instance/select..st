enumerating
select: aBlock
	^ self species newFrom:
		(self associations select: [:each | aBlock value: each value])