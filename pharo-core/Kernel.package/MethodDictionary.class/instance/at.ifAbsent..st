accessing
at: key ifAbsent: aBlock

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	^ array at: index