accessing
searchText: aString
	| index result |
	aString isEmptyOrNil
		ifTrue: [ ^ #() ].
	index := 0.
	result := OrderedCollection new.
	self items
		do: [ :elements | 
			elements
				do: [ :each | 
					index := index + 1.
					((self toString: each) beginsWith: aString)
						ifTrue: [ result add: index ] ] ].
	^ result asArray