parsing
reportField: aString to: aBlock 
	"Evaluate the given block with the field name a value in the given field. Do nothing if the field is malformed."
	| s fieldName fieldValue |
	(aString includes: $:) ifFalse: [ ^ self ].
	s := aString readStream.
	fieldName := (s upTo: $:) asLowercase.	"fieldname must be lowercase"
	fieldValue := s upToEnd trimBoth.
	fieldValue isEmpty ifFalse: 
		[ aBlock 
			value: fieldName
			value: fieldValue ]