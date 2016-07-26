instance creation
creationTimeFromString: aString
	" this is really ugly, but hopefully only a temporary solution untill we come up with somthing more reliable and more beautiful "
	| subexpressions |
	subexpressions := OrderedCollection new.
	'(\d+)-(\d+)-(\d+)T(\d+)-(\d+)-(\d+)(-(\d+))?([-+]\d+)-(\d+)(-(\d+))?' asRegex 
		matchesIn: aString
		subexpressionsDo: [ :expression :expressionFragments | 
			subexpressions addAll: (expressionFragments collect: [ :each | 
				[ each squeezeOutNumber ] on: Error do: [ 0 ] ]) ].
	^ DateAndTime 
		year: subexpressions first 
		month: subexpressions second 
		day: subexpressions third 
		hour: subexpressions fourth 
		minute: subexpressions fifth 
		second: subexpressions sixth 
		nanoSecond: subexpressions eighth * 100000 " 7 == full match "
		offset: (Duration 
						days: 0 
						hours: subexpressions ninth 
						minutes: (subexpressions at: 10) 
						seconds: (subexpressions at: 12)) " 11 == full match "