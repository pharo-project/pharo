accessing
fastPeek
	"Answer a top-most value without removing it from queue.
	Answer nil, if queue is empty or currently blocked by other process,
	which reading from queue"
	
	| item result |
	item := head.
	
	[ (result := item object) == item ] whileTrue: [ 
		item isCircular ifTrue: [ ^ nil ].
		(item := item next) ifNil: [ ^ nil ] ].
	
	^ result