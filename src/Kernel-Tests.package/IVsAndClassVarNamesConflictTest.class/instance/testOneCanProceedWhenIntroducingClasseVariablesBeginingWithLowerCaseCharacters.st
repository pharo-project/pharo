tests
testOneCanProceedWhenIntroducingClasseVariablesBeginingWithLowerCaseCharacters
	| class |
	[ 
		class := classFactory
			newSubclassOf: classFactory defaultSuperclass
			instanceVariableNames: 'a BVarName'
			classVariableNames: ''
	]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (Smalltalk keys includes: class name)