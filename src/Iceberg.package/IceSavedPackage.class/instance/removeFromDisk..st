actions
removeFromDisk: force
	(self isLoaded and: [ force not ]) ifTrue: [ 
		IceError signal: 'You can''t remove a loaded package.' ].
	self location ensureDeleteAll.
	self repository refresh.