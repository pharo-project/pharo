widget API
autoRefresh
	| autoRefresh | 
	
	autoRefresh := self model autoRefresh.
	
	^ (autoRefresh isBlock or: [ autoRefresh isMessageSend ])
		ifTrue: [ autoRefresh cull: self model ]
		ifFalse: [ autoRefresh ]