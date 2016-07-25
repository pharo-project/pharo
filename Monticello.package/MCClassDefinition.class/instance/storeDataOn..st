storing
storeDataOn: aDataStream
	| instVarSize |
	instVarSize := (self hasTraitComposition or: [ self hasClassTraitComposition ])
		ifTrue: [ self class instSize ]
		ifFalse: [ self class instSize - 2 ].
	aDataStream
		beginInstance: self class
		size: instVarSize.
	1 to: instVarSize do: [ :index |
		aDataStream nextPut: (self instVarAt: index) ].