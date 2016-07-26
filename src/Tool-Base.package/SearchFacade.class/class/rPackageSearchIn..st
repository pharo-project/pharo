rpackage search
rPackageSearchIn: anOrganizer
	| rawList |
	rawList := anOrganizer packages sorted: [ :a :b | a name <= b name ].
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e name ] ];
		displayBlock: [ :e | e name ];
		browseBlock: [ :answer | Smalltalk tools browser openOnPackage: answer ];
		title: 'Package search' translated;
		yourself