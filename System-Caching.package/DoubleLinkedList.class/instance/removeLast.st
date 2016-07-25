removing
removeLast
	"Remove my last element.
	Signal a CollectionIsEmpty excpetion when I am empty.
	Return the removed internal link."

	| link |
	self emptyCheck.
	link := tail.
	tail := tail previousLink.
	tail 
		ifNil: [ head := nil ]
		ifNotNil: [ tail nextLink: nil ].
	link clearLinks.
	^ link