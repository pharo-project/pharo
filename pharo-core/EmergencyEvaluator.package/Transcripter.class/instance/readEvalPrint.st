command line
readEvalPrint
	| line okToRevert |
	okToRevert := true.
	Sensor flushKeyboard.
	[#('quit' 'exit' 'done' ) includes: (line := self request: '> ')]
		whileFalse: [line = 'revert'
				ifTrue: [okToRevert
						ifTrue: [(self confirm: 'Revert: ' , RecentMessageList uniqueInstance lastEntry asString , ' ?')
								ifTrue: [RecentMessageList uniqueInstance revertLastMethodSubmission.
									self cr; show: 'reverted: ' , RecentMessageList uniqueInstance lastEntry asString.
									okToRevert := false]]
						ifFalse: [self cr; show: 'Only one level of revert currently supported']]
				ifFalse: [self cr;
						show: ([self class compiler evaluate: line]
								ifError: [:err | err])]]. 
	self cr; show: 'Done'.
