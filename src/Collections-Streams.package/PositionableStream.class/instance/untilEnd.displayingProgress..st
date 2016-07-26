positioning
untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressFrom: 0 to: self size
		during:
			[:bar |
			[self atEnd] whileFalse:
				[bar current: self position.
				aBlock value]].