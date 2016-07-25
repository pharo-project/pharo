private
privateRegisterExternalObject: anObject
	"Register the given object in the external objects array and return its index. If it is already there, just return its index.
	Calls to this method must be guarded against simultaneous removes from multiple threads."

	| objects firstEmptyIndex |
	objects := self unprotectedExternalObjects.

	"find the first empty slot"
	firstEmptyIndex := 
		(self slotFor: anObject in: objects) 
			ifNil: ["if object has no empty slots,  we need to get a collection which does"
				objects := self collectionBasedOn: objects withRoomFor: anObject.
				self slotFor: anObject in: objects.].
	objects at: firstEmptyIndex put: anObject.
	^ firstEmptyIndex
