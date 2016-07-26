copying
copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and 
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection := self copy.
	newCollection add: newElement.
	^newCollection