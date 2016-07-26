*GT-SpotterExtensions-Core-private
allReferencesToSubstring: aString do: aBlock
	| sortedClasses |
	aString isEmptyOrNil ifTrue: [ ^ self ].
	sortedClasses := SortedCollection new sortBlock: [ :a :b | a name size < b name size ].
	(self allBehaviorsDo: [ :class |
		(class name includesSubstring: aString caseSensitive: false) ifTrue: [ 
			sortedClasses add: class ] ]).
	sortedClasses do: [ :class |
		self allReferencesTo: class binding do: aBlock ]