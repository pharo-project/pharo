accessing
shortName
	^ self isLocal 
		ifTrue: [ self basename ]
		ifFalse: [ self name withoutPrefix: 'refs/remotes/']
