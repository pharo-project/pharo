utilities
eventKey: character alt: useAlt ctrl: useCtrl command: useCommand shift: useShift
	| event modifier code |
	event := KeyboardEvent new.
	modifier := 0.
	
	useShift 
		ifTrue: [ modifier := modifier + 8].
	useCtrl 
		ifTrue: [ modifier := modifier + 16].
	useAlt 
		ifTrue: [ modifier := modifier + 32].
	useCommand
		ifTrue: [ modifier := modifier + 64].
	
	code := character asInteger.
	event 
		setType: #keystroke
		buttons: modifier
		position:  0@0
		keyValue: code
		charCode: code
		hand: nil 
		stamp: Time now.
	^ event 	