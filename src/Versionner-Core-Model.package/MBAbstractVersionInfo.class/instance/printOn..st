printing
printOn: aStream
	| cv |
	(cv := self currentVersion) ~~ nil
		ifTrue: [ 
			cv = self version
				ifTrue: [ aStream nextPutAll: ' @ ' ] ].
	super printOn: aStream.
