class analysis
spaceTally: classes
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) asArray"

	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
