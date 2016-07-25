private
collectionBasedOn: externalObjects withRoomFor: anObject
	"Called if no slots to put anObject in have been found in externalObjects "
	"Return a externalObject collection which does, either:
		- Same collection with some slots freed up by finalization logic
		- A larger array, which has replaced the parameter as canonical externalObject array.
	An error must be raised if this method is incapable of fulfilling its duties"
	
	| newObjects newSize |
	"grow linearly"
	newSize :=  externalObjects size +20.
	(self freedSlotsIn: externalObjects ratherThanIncreaseSizeTo: newSize) 
		ifTrue: [newObjects := externalObjects]
		ifFalse: [
			newObjects := externalObjects species new: newSize. 
			"This is only called when adding objects, so caller is responsible for holding ProtectAdd lock.
			Obtain the remove lock as well while swapping the list with a new one, so we don't end up with dead semaphores in newObjects that are removed from externalObjects simultaneously by other threads."
			ProtectRemove critical: [
				newObjects replaceFrom: 1 to:  externalObjects size with: externalObjects startingAt: 1.	
			self unprotectedExternalObjects: newObjects].].
	^newObjects