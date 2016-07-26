enumerating
associationsSelect: aBlock
	^ self species newFrom: (self associations select: aBlock)