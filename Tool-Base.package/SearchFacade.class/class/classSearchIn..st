class search
classSearchIn: classes
	^ ListDialogWindow new
		getList: [ :r | classes select: [ :e | r search: e name ] ];
		displayBlock: [ :e | e name ];
		title: 'Class search' translated;
		yourself