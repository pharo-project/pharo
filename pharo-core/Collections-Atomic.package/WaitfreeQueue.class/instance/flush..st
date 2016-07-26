accessing
flush: aFlushBlock
	"Process all currently available items, passing each item to a flush block.
	If there is another process, which currently fetching items from queue, or queue is empty,
	return immediately"
	
	| item |

	item := dummy makeCircular. 
	item == dummy ifTrue: [ ^ self  ].

	[ | object |
		object := item object.
		object == dummy ifFalse: [
			[ aFlushBlock value: object ] ifCurtailed: [ 
				item object: dummy.
				dummy next: item next ].
		].
		item object: dummy.
		
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: item.
			self signalNoMoreItems.
			^ self
			].
		item := item next.
	] repeat.

	