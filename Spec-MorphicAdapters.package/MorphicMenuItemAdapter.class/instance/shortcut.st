widget API
shortcut
	| shortcut | 
	
	shortcut := self model shortcut.
	
	^ (shortcut isBlock or: [ shortcut isMessageSend ])
		ifTrue: [ shortcut cull: self model ]
		ifFalse: [ shortcut ]