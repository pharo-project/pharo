iterating
allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class and its metaclass"

	self allClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each theMetaClass ] ]