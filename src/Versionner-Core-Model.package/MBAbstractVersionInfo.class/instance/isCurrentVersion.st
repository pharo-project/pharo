testing
isCurrentVersion
	|cv|
	(cv := self currentVersion) ~~ nil
		ifTrue: [ 
			cv = self version
				ifTrue: [^ true]].
	^false.