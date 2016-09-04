protocol
remotes
	| result |
	
	self packages ifEmpty: [ ^ #() ].
	self packages ifEmpty: [ ^ #() ].
	result := self packages first remotes.
	self packages allButFirst do: [ :each |
		result := result intersection: each remotes ].
	
	^ result collect: [ :each | each koRemote ]