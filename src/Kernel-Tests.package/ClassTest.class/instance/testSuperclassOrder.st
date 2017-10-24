tests - file in/out
testSuperclassOrder
	|  ordered orderedSuperclasses shuffledSuperclasses |
	orderedSuperclasses := {ProtoObject. Object. Collection. SequenceableCollection}.
	
	"a shuffled collection of superclasses of OrderedCollection"
	shuffledSuperclasses := {Collection. SequenceableCollection. ProtoObject. Object}.
	
	ordered := Class superclassOrder: shuffledSuperclasses.
	
	"should not affect the order as there is no dependencies"
	self assert: ordered = orderedSuperclasses asOrderedCollection.