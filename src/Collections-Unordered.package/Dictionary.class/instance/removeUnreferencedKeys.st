removing
removeUnreferencedKeys
	"Undeclared removeUnreferencedKeys"

	^ self unreferencedKeys do: [ :key | self removeKey: key ]