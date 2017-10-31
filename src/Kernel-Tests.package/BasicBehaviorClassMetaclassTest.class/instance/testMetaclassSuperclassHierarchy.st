tests
testMetaclassSuperclassHierarchy
	"self run: #testMetaclassSuperclassHierarchy"

	| s |
	self assert: SequenceableCollection class instanceCount  = 1.
	self assert: Collection class instanceCount  = 1.
	self assert: Object class instanceCount  = 1.
	self assert: ProtoObject class instanceCount  = 1.
	
	s := OrderedCollection new.
	s add: SequenceableCollection class.
	s add: Collection class.
	s add: Object class.
	s add: ProtoObject class.

	s add: Class.
	s add: ClassDescription.
	s add: Behavior.
	s add: Object.
	s add: ProtoObject.

	self assert: OrderedCollection class allSuperclasses  = s.



	
	

	