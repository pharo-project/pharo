private
selectEncoding
	"self new selectEncoding"

	| ui |
	ui := ListDialogWindow new
		getList: [ :r | self knownEncodingIdentifiers select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		initialAnswer: '';
		acceptNewEntry: true;
		title: 'Encoding' translated;
		yourself.
	(ui chooseFromOwner: World) ifNotNil: [ :encoding | self fileEncoding: encoding ]