typing support
dispatchCommandOn: aKeyboardEvent return: return
	|asciiValue honorCommandKeys char|

	asciiValue := aKeyboardEvent keyValue.
	honorCommandKeys := self cmdKeysInText.
	"Special keys overwrite crtl+key combinations - at least on Windows. To resolve this
	conflict, assume that keys other than cursor keys aren't used together with Crtl." 
	((self class specialShiftCmdKeys includes: asciiValue) and: [ asciiValue < 27])
		ifTrue: [
			^ self performCmdActionsWith: aKeyboardEvent 
				shifted: aKeyboardEvent controlKeyPressed
				return: return].

	"backspace, and escape keys (ascii 8 and 27) are command keys"
	((honorCommandKeys and: [aKeyboardEvent commandKeyPressed]) or: [self class specialShiftCmdKeys includes: asciiValue]) 
		ifTrue: [
			^ self performCmdActionsWith: aKeyboardEvent 
				shifted: aKeyboardEvent shiftPressed
				return: return].

	"the control key can be used to invoke shift-cmd shortcuts"
	(honorCommandKeys and: [aKeyboardEvent controlKeyPressed])
		ifTrue: [
			^ self performCmdActionsWith: aKeyboardEvent shifted: true return: return].
			
	^ false
