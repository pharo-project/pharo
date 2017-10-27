results
tempNodes 
	| tempNodes |
	tempNodes := SortedCollection sortBlock: [:n1 :n2 | n1 code <= n2 code].
	scopeTable associationsDo:
		[:assn |
		assn value isArray
			ifTrue: [assn value do: [:temp| tempNodes add: temp]]
			ifFalse: [assn value isTemp ifTrue: [tempNodes add: assn value]]].
	^tempNodes