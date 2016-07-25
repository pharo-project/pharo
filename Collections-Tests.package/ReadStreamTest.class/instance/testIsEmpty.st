tests - testing
testIsEmpty
	| stream |
	self assert: self emptyStream isEmpty.
	
	stream := self streamOnArray.
	self deny: stream isEmpty.
	stream skip: 3.
	self deny: stream isEmpty.
	
	stream := self streamOnString.
	self deny: stream isEmpty.
	stream next;next;next.
	self deny: stream isEmpty.
	stream setToEnd.
	self deny: stream isEmpty.