event handling
getMenu: shiftKeyState 
	^ shiftKeyState not
		ifTrue: [TextEditor yellowButtonMenu]
		ifFalse: [TextEditor shiftedYellowButtonMenu]