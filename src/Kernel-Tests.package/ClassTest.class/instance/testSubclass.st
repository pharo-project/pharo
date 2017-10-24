tests - class creation
testSubclass
	| cls |
	(Smalltalk globals includesKey: #SubclassExample)
		ifTrue: [ (Smalltalk at: #SubclassExample) removeFromSystem ].
	
	self deny: (Smalltalk globals includesKey: #SubclassExample).
	cls := Object subclass: #SubclassExample.
	self assert: (Smalltalk globals includesKey: #SubclassExample).

	self assert: (Smalltalk at: #SubclassExample) == cls.
	self assert: cls category = #Unclassified.
	self assert: cls instVarNames = #().
	
	cls removeFromSystem