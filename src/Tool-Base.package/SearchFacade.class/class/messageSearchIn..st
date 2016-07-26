message search
messageSearchIn: aCollection
	| rawList |
	rawList := aCollection sorted: [ :a :b | a selector <= b selector ].
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e selector ] ];
		displayBlock: [ :e | e selector ];
		title: 'Message search' translated;
		yourself