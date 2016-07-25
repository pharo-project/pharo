enumerating
reverseDo: aBlock
	"Override the superclass for performance reasons."

	lastIndex to: firstIndex by: -1 do: [ :index | 
		aBlock value: (array at: index) ]