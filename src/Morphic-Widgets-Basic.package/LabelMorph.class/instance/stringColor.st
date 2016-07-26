accessing
stringColor

	^ self enabled
		ifTrue: [ self color ]
		ifFalse: [ self theme labelDisabledColorFor: self ]