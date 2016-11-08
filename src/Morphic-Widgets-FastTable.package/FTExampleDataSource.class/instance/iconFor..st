private
iconFor: index
	| class |
	class := self elementAt: index.
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	((class
		includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ]))
		and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest
				ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest
				ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest
				ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment
		ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	^ class systemIcon