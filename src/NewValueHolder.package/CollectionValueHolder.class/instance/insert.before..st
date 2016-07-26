protocol
insert: anObject before: spot

	| result |
	result := value insert: anObject before: spot.
	self valueAdded: anObject.
	^ result