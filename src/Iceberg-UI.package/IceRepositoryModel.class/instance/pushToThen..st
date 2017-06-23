actions
pushToThen: aBlock
	| remote |
	remote := self pickRemote: 'Push to'.
	remote ifNil: [ ^ self ].
	self pushTo: remote then: aBlock