running
resultOfSerializeAndMaterialize: anObject

	self serialize: anObject.
	^ self materialized
