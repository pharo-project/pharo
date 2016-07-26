removing
removeFirst
	"Remove my first element.
	Signal a CollectionIsEmpty excpetion when I am empty.
	Return the removed internal link."
	
	| link |
	self emptyCheck.
	link := head.
	head := head nextLink.
	head 
		ifNil: [ tail := nil ]
		ifNotNil: [ head previousLink: nil ].
	link clearLinks.
	^ link