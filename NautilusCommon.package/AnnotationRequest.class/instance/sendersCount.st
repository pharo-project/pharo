requests
sendersCount
	"A report of how many senders there of the message."

	| sendersCount |
	sendersCount := (self systemNavigation allCallsOn: self methodSelector) size.
	^ sendersCount = 1
		ifTrue: [ '1 sender' ]
		ifFalse: [ sendersCount printString , ' senders' ]