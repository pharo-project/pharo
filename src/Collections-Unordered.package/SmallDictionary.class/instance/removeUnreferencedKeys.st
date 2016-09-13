removing
removeUnreferencedKeys
	^ self unreferencedKeys do: [ :key | self removeKey: key ]