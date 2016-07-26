accessing
peekFor: anObject 
	"Answer false and do not move over the next element if it is not equal to 
	the argument, anObject, or if the receiver is at the end. Answer true 
	and increment the position for accessing elements, if the next element is 
	equal to anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject := self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position := position - 1.
	^false