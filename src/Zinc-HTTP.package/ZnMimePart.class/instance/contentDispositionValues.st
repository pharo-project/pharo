private
contentDispositionValues
	| contentDisposition values start semiColonIndex |
	(contentDisposition := self contentDisposition) isNil ifTrue: [ ^ #() ].
	values := OrderedCollection new: 3.
	start := 1.
	semiColonIndex := contentDisposition indexOf: $; startingAt: start ifAbsent: [ 0 ].
	[ semiColonIndex isZero ] whileFalse: [
		| value |
		value := contentDisposition copyFrom: start to: semiColonIndex - 1.
		values add: (ZnUtils trimString: value).
		start := semiColonIndex + 1.
		semiColonIndex := contentDisposition indexOf: $; startingAt: start ifAbsent: [ 0 ] ].
	values add: (ZnUtils trimString: (contentDisposition copyFrom: start to: contentDisposition size)).
	^ values