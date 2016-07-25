widget API
state
	| state | 
	
	state := self model state.
	
	^ (state isBlock or: [ state isMessageSend ])
		ifTrue: [ state cull: self model ]
		ifFalse: [ state ]