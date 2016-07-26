private
compareEqualTo: aMetacelloVersionNumber

	| mySize |
	aMetacelloVersionNumber species = self species ifFalse: [ ^false ].
	mySize := self size.
	mySize = aMetacelloVersionNumber size 
		ifFalse: [ ^false ].
	1 to: mySize do: [:i |
		(self at: i) = (aMetacelloVersionNumber at: i) ifFalse: [ ^false ]].
	^true