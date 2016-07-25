tests
testIsDirty
	"This test is likely to be bogus ... cannot just create a package for packages that don'e exist. see Metacello Issue 113"
	
	| cls |
	self deny: (package isDirty).
	self assert: (package printString =  'FooBarZork ---').
	
	"We create a class"		
	self deny: (Smalltalk includesKey: #TMPClass).
	cls := Object subclass: #TMPClass
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'FooBarZork'.
		
	self assert: (package isDirty).
	"self assert: (package printString =  'FooBarZork ---')."

	(Smalltalk includesKey: #TMPClass)
		ifTrue: [ (Smalltalk at: #TMPClass) removeFromSystem ].
