adding
addFirst: anObjectOrLink
	"Add anObjectOrLink to me, so that it becomes the first one.
	Return the internal link object."
	
	| link |
	link := anObjectOrLink asDoubleLink.
	link nextLink: head.
	head ifNotNil: [ head previousLink: link ].
	tail ifNil: [ tail := link ].
	head := link.
	^ link