accessing-query
queryAt: key add: value
	"Modify the receiver by adding a query variable key=value.
	If key already exists, value is added to it."

	query isNil 
		ifTrue: [ query := ZnMultiValueDictionary new ].
	query at: key asString add: (value ifNotNil: [value asString])