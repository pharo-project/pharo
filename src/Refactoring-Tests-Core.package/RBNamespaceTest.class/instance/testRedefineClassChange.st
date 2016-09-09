class tests
testRedefineClassChange
	| st |
	st := RBClassModelFactory rbNamespace new.
	st defineClass: 'nil subclass: #Object
				instanceVariableNames: ''a''
				classVariableNames: ''A''
				poolDictionaries: ''TextConstants''
				category: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #Object).
	self assert: (st classNamed: #Object) notNil