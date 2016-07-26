accessing
at: anItem
	^ self classPool at: anItem ifAbsent: [ nil ]