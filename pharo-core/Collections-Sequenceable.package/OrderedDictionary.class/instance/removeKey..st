removing
removeKey: aKey
	| value |

	value := dictionary removeKey: aKey.
	self orderedKeysRemove: aKey.
	^ value.