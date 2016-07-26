private
findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."

	| index |

	index := self scanFor: anObject.
	index > 0 ifTrue: [^index].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!'.