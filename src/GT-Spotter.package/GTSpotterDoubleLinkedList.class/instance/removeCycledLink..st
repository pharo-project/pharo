as yet unclassified
removeCycledLink: link
	| predecessor successor |
	predecessor := link previousLink.
	successor := link nextLink.
	predecessor
		ifNil: [ head := successor ]
		ifNotNil: [ predecessor nextLink: successor ].
	successor
		ifNil: [ tail := predecessor ]
		ifNotNil: [ successor previousLink: predecessor ].
	link clearLinks.
	
	"I'm the only one left, clear linked list and return"
	(link = head & (link = tail)) ifTrue: [ head := nil. tail := nil. ^ link ].
	
	link = head ifTrue: [ head := successor ].
	link = tail ifTrue: [ tail := predecessor ].
	
	^ link