testing - ast mapping
testMethodSourceNodeAtInitialPC

	| method actual |
	method := self class >> testSelector.
	actual := method sourceNodeForPC: method initialPC.
	
	self assert: actual equals: method ast sendNodes first receiver receiver
