*ston-core
fromSton: stonReader
	| elements |
	elements := Array streamContents: [ :out |
		stonReader parseListDo: [ :each | out nextPut: each ] ].
	^ self withAll: elements