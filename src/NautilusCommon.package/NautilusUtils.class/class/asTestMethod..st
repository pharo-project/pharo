utilities
asTestMethod: aMethod
	^ aMethod isTestMethod
		ifTrue: [ aMethod ]
		ifFalse: [ aMethod correspondingTestMethod ]