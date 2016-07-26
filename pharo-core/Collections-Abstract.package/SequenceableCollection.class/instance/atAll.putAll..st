accessing
atAll: indexArray putAll: valueArray
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray