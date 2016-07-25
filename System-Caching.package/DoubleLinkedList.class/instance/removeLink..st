removing
removeLink: link
	"Remove the specified link.
	When otherLink is not part of me, the result is undefined.
	Return the removed internal link."

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
	^ link