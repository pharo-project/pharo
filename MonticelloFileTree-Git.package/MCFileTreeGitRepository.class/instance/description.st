descriptions
description
	directory exists
		ifFalse: [ ^ 'deleted' ].
	remoteUrl ifNil: [ ^ self shortDirectoryDescription , ' [' , self branch , '] (GitFileTree)' ].
	self branch ifNotNil: [ ^ self shortRemoteDescription , ' [' , self branch , '] (Remote Git)' ].
	^ remoteUrl