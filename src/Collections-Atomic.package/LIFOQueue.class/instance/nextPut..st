accessing
nextPut: anObject
	
	| newItem oldHead |

	newItem := self newItem.
	newItem object: anObject.

	"this is atomic"
	oldHead := head.
	head := newItem.
	
	newItem next: oldHead.
	
	self signalAddedNewItem.
	^ anObject