accessing
extendedClassNames

	^ self extendedClasses isDictionary
		ifTrue: [ self extendedClasses keys ]
		ifFalse:[ self extendedClasses collect:[ :each| each name ]]