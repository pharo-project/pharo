protocol search
protocolSearchIn: aCollection
	^ ListDialogWindow new
		getList: [ :r | aCollection sorted select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		title: 'Protocol search' translated;
		yourself