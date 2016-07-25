widget API
enabled
	| enabled | 
	
	enabled := self model enabled.
	
	^ (enabled isBlock or: [ enabled isMessageSend ])
		ifTrue: [ enabled cull: self model ]
		ifFalse: [ enabled ]