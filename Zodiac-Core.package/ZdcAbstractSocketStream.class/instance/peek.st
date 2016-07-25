stream in
peek
	"Peek and return a single byte"
	
	readBuffer isEmpty ifTrue: [ self fillReadBuffer ].
	^ readBuffer isEmpty
		ifTrue: [ nil ]
		ifFalse: [ readBuffer peek]