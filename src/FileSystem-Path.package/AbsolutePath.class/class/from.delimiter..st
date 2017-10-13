as yet unclassified
from: aString delimiter: aDelimiterCharater
	aString = '/'
		ifTrue: [ ^ self root ].
		
	^ super from: aString delimiter: aDelimiterCharater