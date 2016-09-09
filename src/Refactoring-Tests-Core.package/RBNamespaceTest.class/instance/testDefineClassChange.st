class tests
testDefineClassChange
	| st |
	st := RBClassModelFactory rbNamespace new.
	st 
		defineClass: 'RefactoringBrowserTest subclass: #SmalltalkTestXXX
				instanceVariableNames: ''''
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #SmalltalkTestXXX).
	self assert: (st classNamed: #SmalltalkTestXXX) notNil