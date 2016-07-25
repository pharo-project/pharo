private
iconFor: index
	| class |
	class := self elementAt: index.
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testRedIcon ].
			^ Smalltalk ui icons iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ Smalltalk ui icons iconNamed: #uncommentedClassIcon ].
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ Smalltalk ui icons iconNamed: #testRedIcon ].
			^ Smalltalk ui icons iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ Smalltalk ui icons iconNamed: #uncommentedClassIcon ].
	^ class systemIcon