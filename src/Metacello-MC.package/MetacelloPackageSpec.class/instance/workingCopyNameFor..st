gofer
workingCopyNameFor: anMCLoader

	| vi |
	(vi := anMCLoader currentVersionInfoFor: self) == nil ifTrue: [ ^nil ].
	^vi isArray ifTrue: [ vi printString ] ifFalse: [ vi name ]