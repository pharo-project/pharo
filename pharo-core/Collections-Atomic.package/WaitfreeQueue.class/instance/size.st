accessing
size
	"Answer the size of queue. 
	If queue is currently in the middle of extraction by other process, give a (very) approximate answer.
	
	This method having a non-deterministic results, because of nature of concurrent shared queue. 
	"
	
	| first item count |

	item := dummy.

	count := 0.
	[ item := item next. item object == dummy ifFalse: [ count := count + 1]. item isCircular ] whileFalse.

	count = 0 ifTrue: [
		"if queue is locked by other process, check the tail and give a (very) approximate answer."
		^ tail object == dummy ifTrue: [ 0 ] ifFalse: [ 1]
		].	

	^ count