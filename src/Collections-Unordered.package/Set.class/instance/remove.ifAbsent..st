removing
remove: oldObject ifAbsent: aBlock

	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject