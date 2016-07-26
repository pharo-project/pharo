querying
packageNamed: aString ifAbsent: aBlock

	| pkg |
	(pkg := self spec packageNamed: aString ifAbsent: []) == nil
		ifTrue: [ ^aBlock value ].
	^pkg referencedSpec