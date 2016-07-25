widget API
icon
	| icon | 
	
	icon := self model icon.
	
	^ (icon isBlock or: [ icon isMessageSend ])
		ifTrue: [ icon cull: self model ]
		ifFalse: [ icon ]