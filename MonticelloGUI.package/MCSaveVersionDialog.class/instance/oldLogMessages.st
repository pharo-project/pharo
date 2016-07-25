morphic ui
oldLogMessages
	| list index |
	list := self previousMessages collect: [:s | s truncateWithElipsisTo: 30].
	list ifEmpty: [UIManager default inform: 'No previous log message was entered'. ^ self].
	index := UIManager default chooseFrom: list.
	
	"no comment was selected"
	index isZero ifTrue: [ ^ self ].
	
	self logMessage: (self previousMessages at: index)