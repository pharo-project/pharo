accessing
bench: messages
	| results |
	results := Dictionary new.
	messages 
		do: [ :each | | bench report |
			bench := self new.
			bench perform: each.
			bench writeRepresentation.
			report := 'Writing {1} - Reading {2}' format: { bench benchWrite. bench benchRead }.
			results at: each put: report ] 
		displayingProgress: 'Benchmarking...'.
	^ results