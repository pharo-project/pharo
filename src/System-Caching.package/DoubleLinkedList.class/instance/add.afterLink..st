adding
add: anObjectOrLink afterLink: otherLink
	"Add anObjectOrLink right after otherLink in me.
	When otherLink is not part of me, the result is undefined.
	Return the internal link object."

	| link otherLinkSuccessor |
	otherLink = tail
		ifTrue: [ ^ self addLast: anObjectOrLink ].
	link := anObjectOrLink asDoubleLink.
	otherLinkSuccessor := otherLink nextLink.
	otherLink nextLink: link.
	link previousLink: otherLink.
	link nextLink: otherLinkSuccessor.
	otherLinkSuccessor previousLink: link.
	^ link