copying
copyWith: newElement
	"Answer a new collection with newElement added (as last
	element if sequenceable)."

	^ self copy
		add: newElement;
		yourself