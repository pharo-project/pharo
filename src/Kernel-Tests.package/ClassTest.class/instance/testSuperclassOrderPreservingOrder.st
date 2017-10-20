tests - file in/out
testSuperclassOrderPreservingOrder
	| noHierarchicalRelationship ordered |
	"a shuffled collection of direct subclasses of Collection"
	noHierarchicalRelationship := {CharacterSet. WideCharacterSet. OrderedDictionary. DependentsArray. Bag. SmallDictionary. SequenceableCollection. HashedCollection. WeakRegistry. Matrix. Heap}.
	
	ordered := Class superclassOrder: noHierarchicalRelationship.
	
	"should not affect the order as there is no dependencies"
	self assert: ordered = noHierarchicalRelationship asOrderedCollection.
	
	