classes and traits
allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system."

	^self classNames do: [:name | aBlock value: (self at: name)]