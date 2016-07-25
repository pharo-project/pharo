accessing
searchText: aString
	| search text result |
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	result := OrderedCollection new.
	search := aString trimBoth asLowercase.
	1 to: self numberOfRows do: [ :rowIndex | 
		text := (self elementWrapped: (self elementAt: rowIndex)) contents trimBoth asLowercase.
		(text beginsWith: search)
			ifTrue: [ result add: rowIndex ] ].
	^ result asArray