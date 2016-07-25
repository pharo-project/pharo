adding
addLast: anObjectOrLink
	"Add anObjectOrLink to me, so that it becomes the last one.
	Return the internal link object."

	| link |
	link := anObjectOrLink asDoubleLink.
	link previousLink: tail.
	tail ifNotNil: [ tail nextLink: link ].
	head ifNil: [ head := link ].
	tail := link.
	^ link