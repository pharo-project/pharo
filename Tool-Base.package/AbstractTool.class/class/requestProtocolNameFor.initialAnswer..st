private
requestProtocolNameFor: aClass initialAnswer: aString
	| ui |
	ui := ListDialogWindow new
		getList: [ :r | (self protocolSuggestionsFor: aClass) select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		initialAnswer: aString;
		acceptNewEntry: true;
		title: 'New Protocol' translated;
		yourself.
	^ ui chooseFromOwner: World