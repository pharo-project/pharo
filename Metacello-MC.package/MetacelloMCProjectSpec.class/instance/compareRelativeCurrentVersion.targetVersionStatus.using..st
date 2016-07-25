testing
compareRelativeCurrentVersion: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader

	| cv vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(cv := self relativeCurrentVersion) == nil ifTrue: [ ^false ].
	(targetVersionStatus includes: cv versionStatus)
		ifTrue: [ ^cv perform: anOperator with: vrsn ].
	^false