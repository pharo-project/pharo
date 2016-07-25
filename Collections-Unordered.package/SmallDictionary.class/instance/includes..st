testing - values
includes: aValue
	self do: [:each | aValue = each ifTrue: [^true]].
	^false