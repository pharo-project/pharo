class tests
testDefineClassChange
	| st |
	st := RBNamespace new.
	st 
		defineClass: 'RefactoringBrowserTest subclass: #SmalltalkTestXXX
				instanceVariableNames: ''''
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #SmalltalkTestXXX).
	self assert: (st classNamed: #SmalltalkTestXXX) notNil