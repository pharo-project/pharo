adding
add: anObjectOrLink beforeLink: otherLink
	"Add anObjectOrLink right before otherLink in me.
	When otherLink is not part of me, the result is undefined.
	Return the internal link object."

	| link otherLinkPredeccessor |
	otherLink = head
		ifTrue: [ ^ self addFirst: anObjectOrLink ].
	link := anObjectOrLink asDoubleLink.
	otherLinkPredeccessor := otherLink previousLink.
	otherLink previousLink: link.
	link nextLink: otherLink.
	link previousLink: otherLinkPredeccessor.
	otherLinkPredeccessor nextLink: link.
	^ link