tests
testCustomAssociations
	| associations |
	associations := OrderedCollection new.
	associations add: #foo->100.
	associations add: (STONTestAssociation key: #foo value:100).
	self serializeAndMaterialize: associations