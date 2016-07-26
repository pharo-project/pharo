protocol
remotesFor: aCollection
	| result |
	
	result := aCollection first remotes.
	aCollection allButFirst do: [ :each |
		result := result intersection: each remotes ].
	
	^ result collect: [ :each | each koRemote ]