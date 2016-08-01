gofer
workingCopyNameFor: anMCLoader

	| vi |
	(vi := anMCLoader currentVersionInfoFor: self) == nil ifTrue: [ ^nil ].
	^vi name