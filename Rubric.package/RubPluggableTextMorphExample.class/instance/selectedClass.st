view creation
selectedClass
	| cls |
	cls := (Smalltalk at: (self selectedClassName getString ifEmpty: [ ^ nil ]) asSymbol ifAbsent: [ ^ nil ])
		theNonMetaClass.
	^ self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls ]