accessing pattern
pattern
	" the pattern to use to filter the list"
	^ self ensureSafePattern: patternModel getText asString trimBoth
