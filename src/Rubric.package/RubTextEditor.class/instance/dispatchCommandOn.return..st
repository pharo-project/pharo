typing support
dispatchCommandOn: aKeyboardEvent return: return
	| asciiValue  |
	asciiValue := aKeyboardEvent keyValue.	
	"Special keys overwrite crtl+key combinations - at least on Windows. To resolve this
	conflict, assume that keys other than cursor keys aren't used together with Crtl."
	((self specialShiftCmdKeys includes: asciiValue) and: [ asciiValue < 27 ])
		ifTrue: [ ^ self performCmdActionsWith: aKeyboardEvent shifted: aKeyboardEvent controlKeyPressed return: return ].	
	
	"backspace, and escape keys (ascii 8 and 27) are command keys"
	(aKeyboardEvent commandKeyPressed or: [ self specialShiftCmdKeys includes: asciiValue ])
		ifTrue: [ ^ self performCmdActionsWith: aKeyboardEvent shifted: aKeyboardEvent shiftPressed return: return ].	
			
	"the control key can be used to invoke shift-cmd shortcuts"
	aKeyboardEvent controlKeyPressed
		ifTrue: [ ^ self performCmdActionsWith: aKeyboardEvent shifted: true return: return ].
	^ false