testing
currentVersionMismatch
	| cv specVersion |
	(cv := self currentVersion) notEmpty
		ifTrue: [ 
			(specVersion := self spec versionOrNil) ~~ nil
				ifTrue: [^ cv asMetacelloVersionNumber ~= specVersion versionNumber ]].
	^false