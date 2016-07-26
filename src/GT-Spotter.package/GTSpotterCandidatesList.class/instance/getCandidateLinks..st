candidates
getCandidateLinks: aProcessorLink
	"returns all candidate links that belongs to a processor link"
	|list head tail current|
	
	head := aProcessorLink headLink.
	tail := aProcessorLink tailLink.
	
	list := OrderedCollection new.
	head ifNil: [ ^ list ].
	(head = tail) ifTrue: [ list add: head. ^ list ].
	
	current := head.
	
	[ current = tail ] whileFalse: [
		list add: current.
		current := current nextLink ].
	"adding last one"
	list add: current.
	
	^ list
	