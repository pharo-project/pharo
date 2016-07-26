classes and traits
allBehaviors
	"Return all the classes and traits defined in the Smalltalk SystemDictionary"

	^ self allClassesAndTraits flatCollect: [:each | {each . each classSide} ]