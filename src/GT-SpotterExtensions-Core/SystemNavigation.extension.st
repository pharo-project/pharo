Extension { #name : #SystemNavigation }

{ #category : #'*GT-SpotterExtensions-Core-private' }
SystemNavigation >> allReferencesTo: aLiteral do: aBlock [
	| symbol |
	symbol := aLiteral isSymbol.
	self allBehaviorsDo: [ :class |
		symbol
			ifFalse: [ class withMethodsReferTo: aLiteral do: aBlock ]
			ifTrue: [ class withThorougMethodsReferTo: aLiteral do: aBlock ] ]
]

{ #category : #'*GT-SpotterExtensions-Core-private' }
SystemNavigation >> allReferencesToSubstring: aString do: aBlock [
	| sortedClasses |
	aString isEmptyOrNil ifTrue: [ ^ self ].
	sortedClasses := SortedCollection new sortBlock: [ :a :b | a name size < b name size ].
	(self allBehaviorsDo: [ :class |
		(class name includesSubstring: aString caseSensitive: false) ifTrue: [ 
			sortedClasses add: class ] ]).
	sortedClasses do: [ :class |
		self allReferencesTo: class binding do: aBlock ]
]

{ #category : #'*GT-SpotterExtensions-Core-private' }
SystemNavigation >> allSendersToString: aString do: aBlock [
	aString isEmptyOrNil ifTrue: [ ^ self ].
	aString asSymbol ifNotNil: [ :symbol |
		self allBehaviorsDo: [ :class | 
			class withThorougMethodsReferTo: symbol do: aBlock ] ]
]
