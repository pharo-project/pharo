accessing
peek
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone but do not remove it from the receiver. 
	If queue is empty or there are other process currently fetching object from queue, answer nil.
	
	Note: do not assume that if #peek answers non-nil object, the next message sent to queue,
	like #next or #nextOrNil will answer the same item.
	The use of this method is discouraged , it is provided only for backward compatibility.
	"

	| item result |

	item := dummy next. 

	[ (result := item object) == dummy ] whileTrue: [
		item isCircular ifTrue: [ 
			self signalNoMoreItems. 
			^ nil ].
		item := item next.
	].
	
	^ result
