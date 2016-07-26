accessing-classes
at: aKey
	^ self at: aKey ifAbsent: [ self error: aKey printString , ' not found' ]