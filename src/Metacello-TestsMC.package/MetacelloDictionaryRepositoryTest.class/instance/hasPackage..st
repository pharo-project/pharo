utilities
hasPackage: aString
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil