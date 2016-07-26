brick-layout
doNotLayoutDuring: aBlock
	|wasAllowed|
	wasAllowed := self isLayotingAllowed.
	self isLayotingAllowed: false.
	[ aBlock cull: self ]
		ensure: [ wasAllowed ifTrue: [ self isLayotingAllowed: true ] ]