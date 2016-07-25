class analysis
systemWideSpaceTally
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"(SpaceTally new systemWideSpaceTally asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) asArray"

	^self spaceTally: Smalltalk globals allClasses.

