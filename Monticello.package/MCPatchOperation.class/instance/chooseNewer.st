accessing
chooseNewer
	self isLocalNewer 
		ifTrue: [ self chooseLocal ]
		ifFalse: [ self isRemoteNewer ifTrue: [ self chooseRemote ]]