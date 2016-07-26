mocks
uuidForName: aName 
	| nm id |
	nm := aName asString.
	id := '00000000-0000-0000-0000-0000000000' 
				, (nm size = 1 ifTrue: [nm , '0'] ifFalse: [nm]).
	^UUID fromString: id