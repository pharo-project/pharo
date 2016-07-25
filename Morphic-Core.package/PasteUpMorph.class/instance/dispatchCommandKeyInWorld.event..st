world menu
dispatchCommandKeyInWorld: aChar event: evt
	"Dispatch the desktop command key if possible.  Answer whether handled"

	| aMessageSend |
	aMessageSend := self commandKeySelectors at: aChar ifAbsent: [^ false].
	aMessageSend selector numArgs = 0
		ifTrue:
			[aMessageSend value]
		ifFalse:
			[aMessageSend valueWithArguments: (Array with: evt)].
	^ true
