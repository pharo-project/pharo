iterating
allTraitsDo: aBlock
	"Evaluate the argument, aBlock, for each trait"

	self allTraits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each theMetaClass ] ]