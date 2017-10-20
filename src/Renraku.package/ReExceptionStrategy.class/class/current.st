accessing
current

	^ current ifNil: [ current := ReLogExceptionStrategy ]