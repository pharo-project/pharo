UI specific
reportOtherProcesses: aBoolean
	self reportOtherProcesses ~= aBoolean
		ifFalse: [ ^ self ].
	reportOtherProcesses := aBoolean.
	self changed: #reportOtherProcesses.
	self updateList