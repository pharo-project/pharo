package search
packageSearchIn: anEnvironment
	| rawList |
	rawList := anEnvironment organization categories sort.
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		title: 'Package search' translated;
		yourself