displaying
displayString
	^ version ifNil:
		[ name ]
			   ifNotNil:
		[ :aVersion | name, ' (', aVersion author , '.', aVersion name asString, ')' ].