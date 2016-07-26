widget API
name
	| name | 
	
	name := self model name.
	
	^ (name isBlock or: [ name isMessageSend ])
		ifTrue: [ name cull: self model ]
		ifFalse: [ name ]