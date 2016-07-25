class and trait names
classAndTraitNames
	"Answer a sorted collection of all class and trait (not including class-traits) names. Do not bother to sort"

	
	^self classNames, self traitNames
	