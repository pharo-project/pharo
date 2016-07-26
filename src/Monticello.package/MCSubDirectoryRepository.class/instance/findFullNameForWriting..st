i/o
findFullNameForWriting: aBaseName
	| possible split prefix fpattern now |
	split := self splitNameVersionExtensionFor: aBaseName.
	fpattern := split first, '*'.
	possible := SortedCollection sortBlock: [ :a :b |
		a first = b first
			ifTrue: [ a second = b second
					ifFalse: [ a second < b second ]
					ifTrue: [ a third fullName size < b third fullName size ]]
			ifFalse: [ a first > b first ] ].
	now := Time totalSeconds.
	prefix := directory pathSegments size.
	self allDirectories do: [:dir | | dirScore fileScore parts |
		parts := dir pathSegments allButFirst: prefix.
		dirScore := (parts select: [ :part | fpattern match: part ]) size.
		fileScore := (dir entries collect: [ :ent |
			(ent isDirectory not and: [ fpattern match: ent name ])
				ifFalse: [ SmallInteger maxVal ]
				ifTrue: [ now - ent modificationTime asSeconds ]]).	"minimum age"
		fileScore := fileScore isEmpty ifTrue: [ SmallInteger maxVal  ]
			ifFalse: [ fileScore min ].
		possible add: { dirScore. fileScore. dir } ].
	
	^ (possible first third) / aBaseName