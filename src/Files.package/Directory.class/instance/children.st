accessing
children

	| entries i entry |
	entries := OrderedCollection new.
	i := 0.
	[(entry := self childEntryAtIndex: i) notNil]
		whileTrue: [ 
			entries add: entry.
			i := i + 1 ].
	^ entries