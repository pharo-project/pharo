removing
remove: oldObject ifAbsent: aBlock

	| index |
	index := self scanFor: oldObject.
	(array at: index) == flag ifTrue: [ ^ aBlock value ].
	array at: index put: flag.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^oldObject