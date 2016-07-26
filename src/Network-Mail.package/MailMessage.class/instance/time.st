accessing
time
	| dateField |
	dateField := (self fieldNamed: 'date' ifAbsent: [ ^0 ]) mainValue.
	^ [self timeFrom: dateField] ifError: [Date today asSeconds].
