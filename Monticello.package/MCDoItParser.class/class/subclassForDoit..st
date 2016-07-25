instance creation
subclassForDoit: aString
	^ self concreteSubclasses detect: [:ea | ea pattern match: aString] ifNone: []