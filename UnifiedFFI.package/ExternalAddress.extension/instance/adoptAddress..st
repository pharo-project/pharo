*UnifiedFFI-private
adoptAddress: anExternalAddress
	anExternalAddress = 0 ifTrue: [ ^ self beNull ].
	1 to: self size do: [ :index | 
		self basicAt: index put: (anExternalAddress at: index) ]