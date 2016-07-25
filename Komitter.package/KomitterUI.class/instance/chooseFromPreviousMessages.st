private
chooseFromPreviousMessages
	| list index |
	
	list := MCSaveVersionDialog previousMessages collect: [:s | s truncateWithElipsisTo: 80 ].
	list ifEmpty: [ 
		UIManager default inform: 'No previous log message was entered'. 
		^ self ].
	
	index := UIManager default chooseFrom: list.
	
	"no comment was selected"
	index isZero ifTrue: [ ^ self ].

	message 
		text: (MCSaveVersionDialog previousMessages at: index);
		takeKeyboardFocus