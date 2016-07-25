*metacello-mc
versionInfoFromVersionNamed: aString

	| versions |
	versions := self dictionary values select: [:version | version info name beginsWith: aString ].
	versions isEmpty ifTrue: [ ^ nil ].
	versions := versions asSortedCollection: [ :a :b |
		([ (a info name copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) <= 
			([ (b info name copyAfterLast: $.) asNumber ] on: Error do: [:ex | ex return: 0 ]) ].
	^ versions last info