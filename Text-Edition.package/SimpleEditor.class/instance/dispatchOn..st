typing support
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."

	| asciiValue honorCommandKeys |

	"We don't support multiple lines. Therefore, we don't process cr as a #normalCharacter:"
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue 
		ifTrue: [
			morph acceptOnCR
				ifTrue:[
					morph acceptContents .
					self closeTypeIn.
					^ true].
			aKeyboardEvent controlKeyPressed ifTrue: [ ^ self normalCharacter: aKeyboardEvent ].
			aKeyboardEvent shiftPressed ifTrue: [ ^ self lf: aKeyboardEvent ].
			aKeyboardEvent commandKeyPressed ifTrue: [ ^ self crlf: aKeyboardEvent ].
			^ self crWithIndent: aKeyboardEvent ].

	honorCommandKeys := self cmdKeysInText.

	"Special keys overwrite crtl+key combinations - at least on Windows. To resolve this
	conflict, assume that keys other than cursor keys aren't used together with Crtl." 
	((self class specialShiftCmdKeys includes: asciiValue) and: [ asciiValue < 27])
		ifTrue: [^ aKeyboardEvent controlKeyPressed
			ifTrue: [self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent]
			ifFalse: [self perform: (CmdActions at: asciiValue + 1) with: aKeyboardEvent]].

	"backspace, and escape keys (ascii 8 and 27) are command keys"
	((honorCommandKeys and: [aKeyboardEvent commandKeyPressed]) or: [self class specialShiftCmdKeys includes: asciiValue]) ifTrue: [
		^ aKeyboardEvent shiftPressed
			ifTrue: [
				self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent]
			ifFalse: [
				self perform: (CmdActions at: asciiValue + 1) with: aKeyboardEvent]].

	"the control key can be used to invoke shift-cmd shortcuts"
	(honorCommandKeys and: [aKeyboardEvent controlKeyPressed])
		ifTrue: [
			^ self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent].

	^ self normalCharacter: aKeyboardEvent