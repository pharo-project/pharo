results
unusedTempNames 
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= self doItInContextName ifTrue: [unused add: name]]].
	^ unused