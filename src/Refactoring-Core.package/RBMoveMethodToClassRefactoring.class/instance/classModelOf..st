transforming
classModelOf: aClass
	| classModel |
	classModel := aClass isMeta 
		ifTrue: [ RBClassModelFactory rbMetaclass named: aClass name ]
		ifFalse: [ RBClassModelFactory rbClass named: aClass name ].
	classModel model: self model.
	^classModel