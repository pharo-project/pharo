geometry testing
containsPoint: aPoint

	| radius other delta xOverY |
	(bounds containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	(bounds width = 1 or: [bounds height = 1])
		ifTrue: [^ true].  "Degenerate case -- code below fails by a bit"

	radius := bounds height asFloat / 2.
	other := bounds width asFloat / 2.
	delta := aPoint - bounds topLeft - (other@radius).
	xOverY := bounds width asFloat / bounds height asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared