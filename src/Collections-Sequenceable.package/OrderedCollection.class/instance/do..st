enumerating
do: aBlock
	"Override the superclass for performance reasons."

	firstIndex to: lastIndex do: [ :index | 
		aBlock value: (array at: index) ]