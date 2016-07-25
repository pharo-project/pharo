accessing
chooseOlder
	self isRemoteNewer 
		ifTrue: [ self chooseLocal ]
		ifFalse: [ self isLocalNewer ifTrue: [ self chooseRemote ]]