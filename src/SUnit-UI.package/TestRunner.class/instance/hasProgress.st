history saving
hasProgress

	result classesTested do: [:cls |
		(cls class includesSelector: #lastStoredRun)
			ifTrue: [^ true]].
	^ false