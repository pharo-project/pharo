descriptions
shortRemoteDescription
	(remoteUrl beginsWith: 'git@')
		ifTrue: [ ^ remoteUrl allButFirst: (remoteUrl indexOf: $:) ]
		ifFalse: [ ^ remoteUrl asZnUrl segments in: [ :s | (s last: 2) first , '/' , s last ] ]