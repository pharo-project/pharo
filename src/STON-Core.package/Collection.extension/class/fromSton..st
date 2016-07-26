*ston-core
fromSton: stonReader
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection