actions
ok
	answer := self model assisted
		ifTrue: [ self text ]
		ifFalse: [ self listSelection ].
	^ super ok