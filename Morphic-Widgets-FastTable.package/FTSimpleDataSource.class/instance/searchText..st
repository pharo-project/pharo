accessing
searchText: aString
	| result |
	aString isEmptyOrNil ifTrue: [ ^ #() ].
	result := OrderedCollection new.
	self elements withIndexDo: [ :each :index |
		((self toString: each) beginsWith: aString)
			ifTrue: [ result add: index ] ].
	^ result asArray