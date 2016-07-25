classes and traits
allClassesAndTraitsDo: aBlock
	"Evaluate the argument, aBlock, for each class and trait in the system."
	
	^self classAndTraitNames do: [:each | aBlock value: (self at: each)]
	
	
