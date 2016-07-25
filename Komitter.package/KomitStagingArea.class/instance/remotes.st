protocol
remotes
	| result |
	
	result := self packages first remotes.
	self packages allButFirst do: [ :each |
		result := result intersection: each remotes ].
	
	^ result collect: [ :each | each koRemote ]