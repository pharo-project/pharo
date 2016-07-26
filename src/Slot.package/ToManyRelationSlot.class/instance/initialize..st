initialization
initialize: anObject
	self write: (RelationSet owner: anObject slot: self) to: anObject. 
	