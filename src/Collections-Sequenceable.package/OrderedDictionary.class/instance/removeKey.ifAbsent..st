removing
removeKey: aKey ifAbsent: aBlock
	| oldSize value |

	oldSize := dictionary size.
	value := dictionary removeKey: aKey ifAbsent: aBlock.
	dictionary size < oldSize
		ifTrue: [self orderedKeysRemove: aKey].
	^ value.