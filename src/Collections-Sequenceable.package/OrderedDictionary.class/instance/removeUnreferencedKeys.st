removing
removeUnreferencedKeys
	^ self unreferencedKeys do: [:each | self removeKey: each]