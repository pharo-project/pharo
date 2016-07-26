accessing
peek
	"answer a top-most value without removing it,  or nil, if queue is empty.
	May block if there's another process reading from queue"
	
	| item result |
	item := head.
	
	[ (result := item object) == item ] whileTrue: [ 
		[ item isCircular ] whileTrue: [ self yield ].
		(item := item next) ifNil: [ ^ nil ] ].
	
	^ result