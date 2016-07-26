accessing
isEmpty
	"Answer true if queue contains at least one element.
	Note, that answer is non-deterministic, because sender could be interrupted at any moment,
	means that even if message answers true, there is no any guarantees that consequent #nextXX messages could 
	retrieve item(s) from queue.
	
	Therefore use of this method is discouraged. 
	This test will work only if sender is the only process, which fetching items from queue. If there are two or more processes which 
	fetching items from queue, using this method makes no sense "
	
	| item |

	item := dummy next.

	[  item object == dummy ] whileTrue: [
		item isCircular ifTrue: [ ^ true ].
		item := item next.
	].
	
	^ false