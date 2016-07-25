execution
execute

	| block correctlyExecuted |
	(self hasBeenExecuted and: [ self runOnce ]) ifTrue: [ ^ self ].
	block := self code isBlock
				ifTrue: [ self code ]
				ifFalse: [[ self class compiler evaluate: self code ]].
	correctlyExecuted := true. 
	block on: Halt, Error do: [ :ex | 
		StartupPreferencesLoader default errors add: {ex. self}.
		ex pass ].
	self hasBeenExecuted: correctlyExecuted.