tests
testOneCanProceedWhenIntroducingCapitalizedInstanceVariables
	| class |
	[ 
		class := classFactory
			newSubclassOf: classFactory defaultSuperclass
			instanceVariableNames: 'X Y'
			classVariableNames: ''
	]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (Smalltalk keys includes: class name)