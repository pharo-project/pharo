actions remotes
removeRemote: aRemote then: aBlock
	(self confirm: (('You are going to remove remote {1}. 
Are you sure?') format: {aRemote}))
		ifFalse: [ ^ self ].
	self repository removeRemote: aRemote.
	aBlock cull: aRemote