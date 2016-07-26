accessing
nextIfNone: aBlock
	"Attempt to fetch the next item from queue. Evaluate a block if attempt is failed i.e. there is no items available or queue is locked by another process"
	
	| item result |

	(item := dummy makeCircular) == dummy ifTrue: [
		"queue is either empty or currently locked by other feeder,
		just give a quick answer that item not found"
		 ^ aBlock value  ].

	[ (result := item object) == dummy ] whileTrue: [
		item isCircular ifTrue: [ 
			self signalNoMoreItems. 
			dummy next: item. ^ aBlock value ].
		item := item next.
	].
	
	item object: dummy.

	dummy next: item next.
	^ result
