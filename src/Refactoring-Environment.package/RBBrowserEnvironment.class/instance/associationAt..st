accessing-classes
associationAt: aKey
	^ self associationAt: aKey ifAbsent: [ self error: aKey printString , ' not found' ]