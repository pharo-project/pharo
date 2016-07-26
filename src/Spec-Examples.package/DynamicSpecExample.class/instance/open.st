protocol
open

	object value ifNil: [ ^ self openOnNil ].
	object value isInteger ifTrue: [ ^ self openOnInteger ].
	object value isString ifTrue: [ ^ self openOnString ]