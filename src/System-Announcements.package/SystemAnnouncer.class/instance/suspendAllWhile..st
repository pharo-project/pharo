announce
suspendAllWhile: aBlock
	| oldSuspended |
	oldSuspended := self isSuspended.
	suspended := true.
	^aBlock ensure: [ suspended := oldSuspended ]