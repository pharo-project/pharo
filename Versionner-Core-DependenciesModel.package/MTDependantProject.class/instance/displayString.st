displaying
displayString
	^ version 
		ifNil: [ displayName ]
		ifNotNil: [ version name 
					ifNil: [ displayName ]
					ifNotNil: [displayName, ' (', version name, ')' ] ]