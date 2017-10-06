private - accessing
keyboardEvent 
	| event |
	event := KeyboardEvent new.
	event 
		setType: #keystroke
		buttons: 0
		position:  0@0
		keyValue: 0
		charCode: 0
		hand: nil 
		stamp: Time now.
	^ event 	