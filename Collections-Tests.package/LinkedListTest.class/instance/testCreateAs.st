tests - creating
testCreateAs
	"Test that a LinkedList can be created by sending message #as: to another collection.
	Implementation note: this method is generic for sequenceable collection and should be traitified."
	| anotherCollection aLinkedList |
	anotherCollection := 1 to: 10.
	aLinkedList := anotherCollection as: LinkedList.
	self assert: (aLinkedList isMemberOf: LinkedList).
	self assert: aLinkedList size equals: anotherCollection size.
	aLinkedList with: anotherCollection do: [:nextElementOfLinkedList :nextElementOfAnotherCollection |
		self assert: nextElementOfLinkedList equals: nextElementOfAnotherCollection]