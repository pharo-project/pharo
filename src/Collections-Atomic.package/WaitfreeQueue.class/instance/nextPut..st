accessing
nextPut: value
	" Add new item to queue "
	| item oldTail |
	
	item := self newItem.
	item object: value.

	" atomic swap here"
	oldTail := tail.
	tail := item.
	
	"self interrupt"
	
	oldTail next: item.

	self signalAddedNewItem.

	^ value